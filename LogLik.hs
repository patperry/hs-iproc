{-# LANGUAGE TupleSections #-}
module LogLik (
    LogLik,
    empty,
    insert,
    
    deviance,
    nullDeviance,
    residDf,
    nullDf,
    score,
    fisher,
    
    ) where
 
import Debug.Trace
 
import Data.List( foldl', partition )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( catMaybes )

import Numeric.LinearAlgebra


import Actor( ReceiverId, SenderId )
import DVars( DVar(..), Context )
import qualified DVars as DVars
import Intervals( IntervalId )
import qualified Intervals as Intervals
import Message( Message(..) )
import Model( DynamicWeight(..), SenderModel, ReceiverModel )
import qualified Model as Model
import Params( Params )
import qualified Params as Params
import qualified SVars as SVars


unionWith' :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith' f m m' =
    foldl' (flip $ uncurry $ Map.insertWith' (flip f))
           m
           (Map.toList m')


data ReceiverLogLik =
    ReceiverLogLik { dynamicWeightPart :: !Double
                   , expectedSendIPart :: !(Map IntervalId Double)
                   , expectedReceiveIPart :: !(Map IntervalId Double)
                   , expectedSendIReceiveICrossPart ::
                        !(Map (IntervalId,IntervalId) Double)
                   }
    deriving (Eq, Show)


emptyRLL :: ReceiverLogLik
emptyRLL =
    ReceiverLogLik 0 Map.empty Map.empty Map.empty
    
singletonRLL :: Double -> DynamicWeight -> ReceiverLogLik
singletonRLL l (DynamicWeight vs p dp) = let
    lp = l * p
    ldp = l * dp
    (ss0,rs0) = partition isSend vs
    (ss, rs) = (map toIntervalId ss0, map toIntervalId rs0)
    
    es = Map.fromList $ [ (s,lp) | s <- ss ]
    er = Map.fromList $ [ (r,lp) | r <- rs ]
    esr = Map.fromList $ [ ((s,r), lp) | s <- ss, r <- rs ]
    
    in ReceiverLogLik ldp es er esr
  where
    isSend (Send _) = True
    isSend (Receive _) = False
    
    toIntervalId (Send i) = i
    toIntervalId (Receive i') = i'

unionRLL :: ReceiverLogLik -> ReceiverLogLik -> ReceiverLogLik
unionRLL (ReceiverLogLik dp1 es1 er1 esr1)
         (ReceiverLogLik dp2 es2 er2 esr2) =
    ReceiverLogLik (dp1 + dp2)
                   (unionWith' (+) es1 es2)
                   (unionWith' (+) er1 er2)
                   (unionWith' (+) esr1 esr2)                   

insertRLL :: Double -> DynamicWeight -> ReceiverLogLik -> ReceiverLogLik
insertRLL l dw rll = unionRLL rll $ singletonRLL l dw

data SenderLogLik =
    SenderLogLik { model :: !SenderModel
                 , value :: !Double
                 , sendCount :: !Int
                 , receiveCount :: !(Map ReceiverId Int)
                 , observedSendI :: !(Map IntervalId Int)
                 , observedReceiveI :: !(Map IntervalId Int)
                 , staticWeightPart :: !Double
                 , receiverLogLik :: !(Map ReceiverId ReceiverLogLik)
                 }
    deriving (Eq, Show)

emptySLL :: SenderModel -> SenderLogLik
emptySLL m = SenderLogLik m 0 0 Map.empty Map.empty Map.empty 0 Map.empty

singletonSLL :: SenderModel -> (Context, [ReceiverId]) -> SenderLogLik
singletonSLL m (c,ts) = let
    dv = Params.dvars $ Model.params m
    rm = Model.receiverModel c m
    s = Model.sender m
    l = length ts
    l' = realToFrac l
    vs = concat [ DVars.lookupDyad c s t dv | t <- ts ]
    (ss0, rs0) = partition isSend vs
    (ss, rs) = (map toIntervalId ss0, map toIntervalId rs0)

    val = foldl' (+) 0 [ log (Model.prob rm t) | t <- ts ]
    sc = l
    rc = Map.fromList $ zip ts (repeat 1)
    os = Map.fromListWith (+) $ zip ss (repeat 1)
    or = Map.fromListWith (+) $ zip rs (repeat 1)
    swp = l' * (fst $ Model.probsParts rm)
    rll = Map.fromList [ (r, singletonRLL l' dw)
                       | (r,dw) <- Model.dynamicPart rm
                       ]

    in SenderLogLik m val sc rc os or swp rll
  where
    isSend (Send _) = True
    isSend (Receive _) = False
    
    toIntervalId (Send i) = i
    toIntervalId (Receive i') = i'
      
unionSLL :: SenderLogLik -> SenderLogLik -> SenderLogLik
unionSLL (SenderLogLik m1 val1 sc1 rc1 os1 or1 swp1 rll1)
         (SenderLogLik m2 val2 sc2 rc2 os2 or2 swp2 rll2) =
    SenderLogLik m1
                 (val1 + val2)
                 (sc1 + sc2)
                 (unionWith' (+) rc1 rc2)
                 (unionWith' (+) os1 os2)
                 (unionWith' (+) or1 or2)
                 (swp1 + swp2)
                 (unionWith' unionRLL rll1 rll2)


insertSLL :: (Context, [ReceiverId]) -> SenderLogLik -> (SenderLogLik, Double)
insertSLL cts sll0 = let
    sll1 = singletonSLL (model sll0) cts
    sll = unionSLL sll0 sll1
    resid = -2 * value sll1
    in resid `seq` (sll, resid)

data LogLik =
    LogLik { params :: !Params
           , senderLogLik :: !(Map SenderId SenderLogLik)
           }
    deriving (Eq, Show)

empty :: Params -> LogLik
empty p = LogLik p Map.empty

insert :: (Context, Message) -> LogLik -> (LogLik, Double)
insert (c, (Message f ts)) (LogLik p sllm) = let
    sll0 = Map.findWithDefault (emptySLL $ Model.senderModel p f) f sllm
    (sll,resid) = insertSLL (c,ts) sll0
    sllm' = Map.insert f sll sllm
    in resid `seq` (LogLik p sllm', resid)


data Score = Score !(Vector Double) !(Vector Double) !(Vector Double)
data Fisher = Fisher !(Matrix Double) !(Matrix Double) !(Matrix Double)

deviance :: LogLik -> Double
deviance (LogLik _ sllm) =
    -2 * foldl' (+) 0 [ value sll | sll <- Map.elems sllm ]

nullDeviance :: LogLik -> Double
nullDeviance (LogLik p sllm) =
    2 * foldl' (+) 0
        [ ( realToFrac (sendCount sll)
          * log (realToFrac $ length $ Params.receivers s p)
          )
        | (s,sll) <- Map.assocs sllm
        ]

nullDf :: LogLik -> Int
nullDf (LogLik _ sllm) =
    foldl' (+) 0 [ sendCount sll | sll <- Map.elems sllm ]

residDf :: LogLik -> Int
residDf ll@(LogLik p _) = 
    ( nullDf ll
    -
      ( SVars.dim sv
      + Intervals.size (DVars.sendIntervals dv)
      + Intervals.size (DVars.receiveIntervals dv)
      )
    )
  where
    sv = Params.svars p
    dv = Params.dvars p

score :: LogLik -> Score
score = undefined

fisher :: LogLik -> Fisher
fisher = undefined

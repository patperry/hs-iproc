{-# LANGUAGE TupleSections #-}
module LogLik (
    LogLik,
    empty,
    insert,
    
    deviance,
    nullDeviance,
    residDf,
    nullDf,
    
    Score(..),
    score,
    fisher,
    
    ) where
 
import Debug.Trace
 
import Data.List( foldl', partition )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( catMaybes, fromMaybe )

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
                   , expectedDVarPart :: !(Map DVar Double)
                   , expectedDVarCrossPart :: !(Map (DVar,DVar) Double)
                   }
    deriving (Eq, Show)


emptyRLL :: ReceiverLogLik
emptyRLL =
    ReceiverLogLik 0 Map.empty Map.empty
    
singletonRLL :: Double -> DynamicWeight -> ReceiverLogLik
singletonRLL l (DynamicWeight vs p dp) = let
    lp = l * p
    ldp = l * dp
    
    e = Map.fromListWith (+) $ zip vs (repeat lp)
    e2 = Map.fromListWith (+) $ [ ((v,v'), lp) | v <- vs, v' <- vs]
    
    in ReceiverLogLik ldp e e2


unionRLL :: ReceiverLogLik -> ReceiverLogLik -> ReceiverLogLik
unionRLL (ReceiverLogLik dp1 e1 ecross1)
         (ReceiverLogLik dp2 e2 ecross2) =
    ReceiverLogLik (dp1 + dp2)
                   (unionWith' (+) e1 e2)
                   (unionWith' (+) ecross1 ecross2)

insertRLL :: Double -> DynamicWeight -> ReceiverLogLik -> ReceiverLogLik
insertRLL l dw rll = unionRLL rll $ singletonRLL l dw

data SenderLogLik =
    SenderLogLik { model :: !SenderModel
                 , value :: !Double
                 , sendCount :: !Int
                 , receiveCount :: !(Map ReceiverId Int)
                 , observedDVars :: !(Map DVar Int)
                 , staticWeightPart :: !Double
                 , receiverLogLik :: !(Map ReceiverId ReceiverLogLik)
                 }
    deriving (Eq, Show)

emptySLL :: SenderModel -> SenderLogLik
emptySLL m = SenderLogLik m 0 0 Map.empty Map.empty 0 Map.empty

singletonSLL :: SenderModel -> (Context, [ReceiverId]) -> SenderLogLik
singletonSLL m (c,ts) = let
    val = foldl' (+) 0 [ log (Model.prob rm t) | t <- ts ]
    sc = l
    rc = Map.fromList $ zip ts (repeat 1)
    obs = Map.fromListWith (+) $ zip vs (repeat 1)
    swp = l' * (fst $ Model.probsParts rm)
    rll = Map.fromList [ (r, singletonRLL l' dw)
                       | (r,dw) <- Model.dynamicPart rm
                       ]

    in SenderLogLik m val sc rc obs swp rll
  where
    dv = Params.dvars $ Model.params m
    rm = Model.receiverModel c m
    s = Model.sender m
    l = length ts
    l' = realToFrac l
    vs = concat [ DVars.lookupDyad c s t dv | t <- ts ]
      
      
unionSLL :: SenderLogLik -> SenderLogLik -> SenderLogLik
unionSLL (SenderLogLik m1 val1 sc1 rc1 obs1 swp1 rll1)
         (SenderLogLik m2 val2 sc2 rc2 obs2 swp2 rll2) =
    SenderLogLik m1
                 (val1 + val2)
                 (sc1 + sc2)
                 (unionWith' (+) rc1 rc2)
                 (unionWith' (+) obs1 obs2)
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



data Score = Score { sscore :: !(Vector Double)
                   , dscore :: !(Vector Double) } deriving (Eq, Show)

senderScore :: SenderLogLik -> Score
senderScore sll = let
    o_svars = SVars.sumWithSender s
                  [ (r, realToFrac w)
                  | (r,w) <- Map.assocs $ receiveCount sll ] sv

    e_svars = SVars.sumWithSender s
                  [ (r, staticWeightPart sll
                        + (fromMaybe 0 . fmap dynamicWeightPart . Map.lookup r)
                              (receiverLogLik sll) )
                  | r <- rs ] sv
    
    o_dvars = fromAssocs [ (v, realToFrac c)
                         | (v,c) <- Map.assocs $ observedDVars sll ]
    
    e_dvars = fromAssocs $ Map.assocs $
                  foldl' (flip $ unionWith' (+)) Map.empty
                      [ expectedDVarPart rll
                      | rll <- Map.elems $ receiverLogLik sll ]
    in Score (o_svars `subVector` e_svars)
             (o_dvars `subVector` e_dvars)
  where
    s = Model.sender $ model sll
    rs = Params.receivers s $ Model.params $ model sll
    sv = Params.svars $ Model.params $ model sll
    dv = Params.dvars $ Model.params $ model sll
    
    fromAssocs vws = accumVector (+) (constantVector (DVars.dim dv) 0) $
        [ (DVars.index v dv, w) | (v,w) <- vws ]
    

score :: LogLik -> Score
score ll = 
    foldl' (\(Score s1 d1) (Score s2 d2) ->
                Score (addVector s1 s2) (addVector d1 d2))
           emptyScore
           [ senderScore sll | sll <- Map.elems $ senderLogLik ll ]
  where
    sv = Params.svars $ params ll
    dv = Params.dvars $ params ll
    emptyScore = Score (constantVector (SVars.dim sv) 0)
                       (constantVector (DVars.dim dv) 0)


data Fisher =
    Fisher { ssfisher :: !(Matrix Double)
           , sdfisher :: !(Matrix Double)
           , ddfisher :: !(Matrix Double)
           } deriving (Eq, Show)

{-
data ReceiverLogLik =
    ReceiverLogLik { dynamicWeightPart :: !Double
                   , expectedDVarPart :: !(Map DVar Double)
                   , expectedDVarCrossPart :: !(Map (DVar,DVar) Double)
                   }
    deriving (Eq, Show)

data SenderLogLik =
    SenderLogLik { model :: !SenderModel
                 , value :: !Double
                 , sendCount :: !Int
                 , receiveCount :: !(Map ReceiverId Int)
                 , observedDVars :: !(Map DVar Int)
                 , staticWeightPart :: !Double
                 , receiverLogLik :: !(Map ReceiverId ReceiverLogLik)
                 }
    deriving (Eq, Show)
-}

fisher :: LogLik -> Fisher
fisher = undefined


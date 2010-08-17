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
    Fisher(..),
    fisher,
    fisherWithScore,
    
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



data Score =
    Score { sscore :: !(Vector Double)
          , dscore :: !(Vector Double) }
    deriving (Show)

data Fisher =
    Fisher { ssfisher :: !(Herm Matrix Double)
           , sdfisher :: !(Matrix Double)
           , ddfisher :: !(Herm Matrix Double)
           }
    deriving (Show)

senderScore :: SenderLogLik -> Score
senderScore = fst . senderFisherWithScore

senderFisher :: SenderLogLik -> Fisher
senderFisher = snd . senderFisherWithScore

senderFisherWithScore :: SenderLogLik -> (Score, Fisher)
senderFisherWithScore sll = let
    (rs,xs) = unzip $ SVars.lookupSender s sv
    cs = [ (fromIntegral . Map.findWithDefault 0 r) (receiveCount sll) | r <- rs ]

    ws = [ staticWeightPart sll * Model.prob p0 r
           + (fromMaybe 0 . fmap dynamicWeightPart . Map.lookup r) rll | r <- rs ]
    
    o_sv = weightedSumVector $ zip cs xs
    e_sv = weightedSumVector $ zip ws xs
    cov_sv = mapHerm (scaleMatrix (fromIntegral $ sendCount sll)) $
                 weightedCovMatrix MLCov $ zip ws xs

    o_dv = fromAssocs [ (v, fromIntegral c)
                      | (v,c) <- Map.assocs $ observedDVars sll ]
    
    e_dv = fromAssocs $ Map.assocs $
               foldl' (flip $ unionWith' (+)) Map.empty
                      [ expectedDVarPart rll
                      | rll <- Map.elems $ receiverLogLik sll ]

    e_dv2 = fromAssocs2 $ Map.assocs $
                foldl' (flip $ unionWith' (+)) Map.empty
                       [ expectedDVarCrossPart rll
                       | rll <- Map.elems $ receiverLogLik sll ]

    cov_dv = Herm Upper $ rank1UpdateMatrix (-1) e_dv e_dv e_dv2
    
    e_svdv = matrixViewVector (SVars.dim sv, DVars.dim dv) $ sumVector $
                 (constantVector (SVars.dim sv * DVars.dim dv) 0):
                 [ kroneckerVector
                       (fromAssocs $ Map.assocs $ expectedDVarPart rll)
                       (SVars.lookupDyad s r sv)
                 | (r,rll) <- Map.assocs $ receiverLogLik sll ]
    cov_svdv = rank1UpdateMatrix (-1) e_sv e_dv e_svdv
    
    cov_svdv' = matrixViewVector (SVars.dim sv, DVars.dim dv) $ sumVector $
                 (constantVector (SVars.dim sv * DVars.dim dv) 0):
                 [ kroneckerVector
                       (fromAssocs $ Map.assocs $ expectedDVarPart rll)
                       (SVars.lookupDyad s r sv `subVector` e_sv)
                 | (r,rll) <- Map.assocs $ receiverLogLik sll ] :: Matrix Double
                 
    score = Score (o_sv `subVector` e_sv) (o_dv `subVector` e_dv)
    fisher = Fisher cov_sv cov_svdv' cov_dv
    
    in (score, fisher)
  where
    s = Model.sender $ model sll
    p = Model.params $ model sll
    p0 = Model.staticReceiverModel $ model sll
    sv = Params.svars p
    dv = Params.dvars p
    rll = receiverLogLik sll
    
    fromAssocs vws = accumVector (+)
        (constantVector (DVars.dim dv) 0) $
            [ (DVars.index v dv, w) | (v,w) <- vws ]
    
    fromAssocs2 vvws = accumMatrix (+)
        (constantMatrix (DVars.dim dv, DVars.dim dv) 0) $
            [ ((DVars.index v dv, DVars.index v' dv), w) | ((v,v'),w) <- vvws ]

    mapHerm f (Herm u a) = Herm u $ f a



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

fisherWithScore :: LogLik -> (Score, Fisher)
fisherWithScore ll = 
    foldl' (\((Score s1 d1), (Fisher ss1 sd1 dd1))
             ((Score s2 d2), (Fisher ss2 sd2 dd2)) ->
                let score = Score (addVector s1 s2)
                                  (addVector d1 d2)
                    fisher = Fisher (zipHerm addMatrix ss1 ss2)
                                    (addMatrix sd1 sd2)
                                    (zipHerm addMatrix dd1 dd2)
                in score `seq` fisher `seq` (score, fisher)
           )
           (emptyScore, emptyFisher)
           [ senderFisherWithScore sll | sll <- Map.elems $ senderLogLik ll ]
  where
    sv = Params.svars $ params ll
    dv = Params.dvars $ params ll
    emptyScore = Score (constantVector (SVars.dim sv) 0)
                       (constantVector (DVars.dim dv) 0)
    emptyFisher = Fisher (Herm Upper $ constantMatrix (SVars.dim sv, SVars.dim sv) 0)
                         (constantMatrix (SVars.dim sv, DVars.dim dv) 0)
                         (Herm Upper $ constantMatrix (DVars.dim dv, DVars.dim dv) 0)
    zipHerm f (Herm u a) (Herm _ b) = Herm u (f a b)

fisher :: LogLik -> Fisher
fisher = snd . fisherWithScore

{-# LANGUAGE TupleSections #-}
module SenderLogLik (
    ) where
 
import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( catMaybes )

import Actor( ReceiverId )
import qualified DVars as DVars
import Intervals( IntervalId )
import SenderModel( SenderModel )
import qualified SenderModel as SenderModel


data SenderLogLik =
    SenderLogLik { value :: !Double
                 , count :: !Int
                 , sendCount :: !(Map ReceiverId Int)
                 , observedSendI :: !(Map IntervalId Int)
                 , observedReceiveI :: !(Map IntervalId Int)
                 , staticWeight :: !Double
                 , weightDiffs :: !(Map ReceiverId Double)
                 , expectedSendI :: !(Map IntervalId Double)
                 , expectedReceiveI :: !(Map IntervalId Double)
                 , expectedSendIReceiveICross :: !(Map (IntervalId,IntervalId) Double)
                 }
    deriving (Eq, Show)

empty :: SenderLogLik
empty = SenderLogLik { value = 0
                     , count = 0
                     , sendCount = Map.empty
                     , observedSendI = Map.empty
                     , observedReceiveI = Map.empty
                     , staticWeight = 0
                     , weightDiffs = Map.empty
                     , expectedSendI = Map.empty
                     , expectedReceiveI = Map.empty
                     , expectedSendIReceiveICross = Map.empty
                     }

singleton :: (SenderModel, [ReceiverId]) -> SenderLogLik
singleton (sm,ts) = let
    v = foldl' (+) 0 $ map (`SenderModel.logProb` sm) ts
    
    c = l
    
    sc = Map.fromListWith (+) $ zip ts (repeat 1)
    
    os = Map.fromListWith (+) $ flip zip (repeat 1) $ catMaybes
             [ DVars.lookupDyad (f,t) dv >>= DVars.send
             | t <- ts ]
                  
    or = Map.fromListWith (+) $ flip zip (repeat 1) $ catMaybes
             [ DVars.lookupDyad (f,t) dv >>= DVars.receive
             | t <- ts ]

    sw = fromIntegral l / w_sum

    wd = Map.fromList $ [ let d' = fromIntegral l * (d / w_sum) in d' `seq` (r,d')
                        | (r,d) <- SenderModel.weightDiffs sm ]

    es = Map.fromListWith (+) $ catMaybes
             [ DVars.send d >>= \i -> Just (i,p)
             | (d,p) <- dps ]

    er = Map.fromListWith (+) $ catMaybes
             [ DVars.receive d >>= \i' -> Just (i',p)
             | (d,p) <- dps ]
             
    esrc = Map.fromListWith (+) $ catMaybes
               [ DVars.sendReceive d >>= \ii' -> Just (ii',p)
               | (d,p) <- dps ]
    
    in SenderLogLik { value = v
                    , count = c
                    , sendCount = sc
                    , observedSendI = os
                    , observedReceiveI = or
                    , staticWeight = sw
                    , weightDiffs = wd
                    , expectedSendI = es
                    , expectedReceiveI = er
                    , expectedSendIReceiveICross = esrc
                    }
  where
    l = length ts
    f = SenderModel.sender sm
    dv = SenderModel.dvars sm
    dps = SenderModel.dvarProbs sm
    w_sum = SenderModel.weightSum sm


union :: SenderLogLik -> SenderLogLik -> SenderLogLik
union s1 s2 = let
    v = (+) (value s1) (value s2)
    c = (+) (count s1) (count s2)
    sc = unionWith' (+) (sendCount s1) (sendCount s2)
    os = unionWith' (+) (observedSendI s1) (observedSendI s2)
    or = unionWith' (+) (observedReceiveI s1) (observedReceiveI s2)
    sw = (+) (staticWeight s1) (staticWeight s2)
    wd = unionWith' (+) (weightDiffs s1) (weightDiffs s2)
    es = unionWith' (+) (expectedSendI s1) (expectedSendI s2)
    er = unionWith' (+) (expectedReceiveI s1) (expectedReceiveI s2)
    esrc = unionWith' (+) (expectedSendIReceiveICross s1)
                (expectedSendIReceiveICross s2)

    in SenderLogLik { value = v
                    , count = c
                    , sendCount = sc
                    , observedSendI = os
                    , observedReceiveI = or
                    , staticWeight = sw
                    , weightDiffs = wd
                    , expectedSendI = es
                    , expectedReceiveI = er
                    , expectedSendIReceiveICross = esrc
                    }
      
  where
    unionWith' f m m' =
        foldl' (flip $ uncurry $ Map.insertWith' (flip f))
               m
               (Map.toList m')

insert :: (SenderModel, [ReceiverId]) -> SenderLogLik -> SenderLogLik
insert smr sl = union sl $ singleton smr

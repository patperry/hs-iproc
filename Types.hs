module Types
  where

import Control.Monad( forM_, liftM2 )
import Control.Monad.ST
import Data.Maybe( listToMaybe )
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap

import Numeric.LinearAlgebra




data STDynDeriv s =
    STDynDeriv { sendGrad :: !(STVector s Double)
               , recvGrad :: !(STVector s Double)
               , sendRecvCross :: !(STMatrix s Double)
               }

updateDynDeriv :: (RVector v)
               => v Double -> (History, History) -> STDynDeriv s -> ST s ()
updateDynDeriv prob (sh,rh) (STDynDeriv send recv cross) = do
    forM_ (IntMap.toList $ recentEvents sh) $ \(j,(_,s)) -> do
        p <- readVector prob j
        updateVector send s (p+)
        maybe (return ()) (\r -> updateMatrix cross (s,r) (p+)) $
            snd `fmap` (IntMap.lookup j $ recentEvents rh)
    
    forM_ (IntMap.toList $ recentEvents rh) $ \(j,(_,r)) -> do
        p <- readVector prob j
        updateVector recv r (p+)
      

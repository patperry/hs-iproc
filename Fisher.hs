module Fisher (
    fromMessages
    ) where

import Numeric.LinearAlgebra

import History( History )
import Model( Model )
import Types

import qualified Model as Model
import qualified Vars as Vars

        
fromMessages :: Model -> [(Message, History)] -> Herm Matrix Double
fromMessages m mhs0 = runHermMatrix $ do
    f <- newMatrix (p,p) 0
    go f mhs0
  where
    p = Vars.dim $ Model.vars m
    
    go f [] = return $ Herm defaultCovUplo f
    go f ((msg,h):mhs) = let
        (Herm _ f') = Model.covVars m h (messageFrom msg)
        l = fromIntegral $ length $ messageTo msg
        in do
            addToMatrixWithScales 1 f l f' f
            go f mhs

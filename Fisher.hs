module Fisher
    ( fromMessages
    , invWithTol
    ) where

import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Matrix as M
import qualified Numeric.LinearAlgebra.Vector as V

import History( History )
import Model( Model )
import Types

import qualified Model as Model
import qualified Vars as Vars

        
fromMessages :: Model -> [(Message, History)] -> Herm Matrix Double
fromMessages m mhs0 = M.hermCreate $ do
    f <- M.new (p,p) 0
    go f mhs0
  where
    p = Vars.dim $ Model.vars m
    
    go f [] = return $ Herm M.defaultCovUplo f
    go f ((msg,h):mhs) = let
        (Herm _ f') = Model.covVars m h (messageFrom msg)
        l = fromIntegral $ length $ messageTo msg
        in do
            M.addWithScaleM_ l f' f
            go f mhs

invWithTol :: Double -> Herm Matrix Double -> (Herm Matrix Double, Int)
invWithTol tol a@(Herm uplo _) = let
    (val,vec) = M.hermEigen a
    nzero = length $ [ e | e <- V.elems val, abs e < tol ]
    val' = V.drop nzero val
    val_inv' = V.recip val'
    vec' = M.dropCols nzero vec
    ainv = M.mulMatrix NoTrans vec'
                       Trans   (M.scaleCols val_inv' vec')
    in (Herm uplo ainv, nzero)

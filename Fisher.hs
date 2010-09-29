module Fisher
    ( fromMessages
    , invWithTol
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

invWithTol :: Double -> Herm Matrix Double -> (Herm Matrix Double, Int)
invWithTol tol a@(Herm uplo _) = let
    (val,vec) = eigenHermMatrix a
    nzero = length $ [ e | e <- elemsVector val, abs e < tol ]
    val' = dropVector nzero val
    val_inv' = recipVector val'
    vec' = dropColsMatrix nzero vec
    ainv = mulMatrixMatrix NoTrans vec'
                           Trans   (scaleColsMatrix val_inv' vec')
    in (Herm uplo ainv, nzero)

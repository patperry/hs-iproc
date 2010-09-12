module Fit (
    Result(..),
    fromMessages,
    fromMessagesWithFixed,
    ) where

import Data.List( foldl', nub, sort, (\\) )
import Debug.Trace( trace )
import Numeric.LinearAlgebra

import Types
import History( History )
import LogLik( LogLik )
import Model( Model )

import BFGS( Result(..) )
import qualified BFGS as BFGS
import qualified LineSearch as LineSearch
import qualified Intervals as Intervals
import qualified Fisher as Fisher
import qualified History as History
import qualified Model as Model
import qualified LogLik as LogLik
import qualified Summary as Summary
import qualified Vars as Vars

fromMessages :: Model -> [(Message, History)]
             -> Maybe (Double, Result LogLik)
fromMessages = fromMessagesWithFixed []

fromMessagesWithFixed :: [Int] -> Model -> [(Message, History)]
                      -> Maybe (Double, Result LogLik)
fromMessagesWithFixed fixed m0 mhs = let
    v = Model.vars m0
    smry = Summary.fromMessages v mhs
    notfixed = [ 0..Vars.dim v - 1 ] \\ fixed
    beta0 = Model.coefs m0
    xdim = length notfixed
    x0 = listVector xdim [ atVector beta0 i | i <- notfixed ]
    fdf x = let beta = replaceVector beta0 $ zip notfixed (elemsVector x)
                m = Model.fromVars v beta $ Model.loops m0
                ll = LogLik.fromMessages m mhs
                (val,grad) = LogLik.valueGrad ll
                gradx = listVector xdim [ atVector grad i | i <- notfixed ]
            in ( -val
               , scaleVector (-1) gradx
               , ll
               )
    penalty0 = 1e-1 / (fromIntegral $ Summary.count smry)
    in optWithPenaltyShrink 0.01 penalty0 fdf x0 (fdf x0)

optWithPenalty :: Double
               -> (Vector Double -> (Double, Vector Double, a))
               -> Vector Double
               -> (Double, Vector Double, a)
               -> Either (BFGS.Warning, Result a)
                         (Result a)
optWithPenalty penalty fdf x0 (f0,df0,a0) = let
    f' x = let (f,df,a) = fdf x
           in  ( f + 0.5 * penalty * x `dotVector` x
               , addVectorWithScales 1 df penalty x
               , a
               )
    f0'  = f0 + 0.5 * penalty * x0 `dotVector` x0
    df0' = addVectorWithScales 1 df0 penalty x0
    
    opt = BFGS.minimize BFGS.defaultControl{
                                BFGS.linesearchControl =
                                      LineSearch.defaultControl{
                                            LineSearch.verbose = True
                                          , LineSearch.valueTol = 1e-4
                                          , LineSearch.derivTol = 0.9
                                          , LineSearch.stepMin = 1e-10
                                      }
                                , BFGS.relTol = 1e-6
                                , BFGS.iterMax = 10000
                                , BFGS.verbose = True }
                          f' x0 (f0',df0',a0)

    in case opt of
           Left (w,r) -> Left (w, unPenalty r)
           Right r    -> Right (unPenalty r)
  where
    unPenalty r = let
        x  = BFGS.resultPos r
        f  = BFGS.resultValue r
        df = BFGS.resultGrad r
        in r{ BFGS.resultValue = f - 0.5 * penalty * x `dotVector` x
            , BFGS.resultGrad  = addVectorWithScales 1 df (-penalty) x
            }

optWithPenaltyShrink :: Double
                     -> Double
                     -> (Vector Double -> (Double, Vector Double, a))
                     -> Vector Double
                     -> (Double, Vector Double, a)
                     -> Maybe (Double, Result a)
optWithPenaltyShrink = go Nothing
  where
    go prev shrink penalty0 fdf x0 fdf0@(f0,df0,_) = let
        report r =
            trace ("\n\npenalty: " ++ (show $ penalty0)
                   ++ "\nvalue: " ++ show (BFGS.resultValue r)
                   ++ "\ngrad norm: " ++ show (norm2Vector $ BFGS.resultGrad r)
                   ++ "\ngrad: " ++ show (elemsVector $ BFGS.resultGrad r)
                   ++ "\nposition: " ++ show (elemsVector $ BFGS.resultPos r)
                   ++ "\n\n")
        in case optWithPenalty penalty0 fdf x0 fdf0 of
            Left (w,r) -> report r prev
            Right r -> report r $ let
                next = go (Just (penalty0, r))
                          shrink
                          (shrink*penalty0)
                          fdf
                          (BFGS.resultPos r)
                          ( BFGS.resultValue r
                          , BFGS.resultGrad r
                          , BFGS.resultState r
                          )
                in case prev of
                       Nothing -> next
                       Just (_,r0) -> let
                           diff = abs (BFGS.resultValue r
                                       - BFGS.resultValue r0)
                           in if (diff < 1e-6 * abs (BFGS.resultValue r)
                                      || diff < 1e-16)
                                  then Just (penalty0, r)
                                  else next

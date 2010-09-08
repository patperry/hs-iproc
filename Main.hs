{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main
    where

import Debug.Trace( trace )
import Data.List( foldl', nub, sort, (\\) )
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
        
import Numeric.LinearAlgebra
       
        
import Enron
import History( History )
import Intervals( Intervals )
import LogLik( LogLik )
import Model( Model )
import Types

import qualified BFGS as BFGS
import qualified LineSearch as LineSearch
import qualified Intervals as Intervals
import qualified History as History
import qualified Model as Model
import qualified LogLik as LogLik
import qualified Summary as Summary
import qualified Vars as Vars

     
fromEmail :: Email -> (Time, Message)
fromEmail (Email _ _ time _ f ts) =
    (time, Message f ts)

fromEmployee :: Bool -> Employee -> (ActorId, Vector Double)
fromEmployee intercept (Employee eid _ _ _ gen sen dep) =
    let f = if gen == Female then 1 else 0
        m = if gen == Male then 1 else 0
        j = if sen == Junior then 1 else 0
        s = if sen == Senior then 1 else 0
        l = if dep == Legal then 1 else 0
        t = if dep == Trading then 1 else 0
        o = if dep == Other then 1 else 0
        xs = [ f*j*l, m*j*l, f*s*l, m*s*l
             , f*j*t, m*j*t, f*s*t, m*s*t
             , f*j*o, m*j*o, f*s*o, m*s*o
             ]
        --xs = withInt [ f, j, l, t, f*j, f*l, f*t, j*l, j*t, f*j*l, f*j*t ]
        p = length xs
    in (eid, listVector p xs)
  where
    withInt = if intercept then (1:) else id

sendIntervals :: Intervals
sendIntervals = Intervals.fromList []
{-
sendIntervals :: Intervals
sendIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $    
        [ -7..2 ] ++ [ 4..13 ]
-}

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $
        [ -6..14 ]

{-
fromEmployee :: Employee -> (ActorId, Actor)
fromEmployee (Employee eid _ _ _ g s d) =
    let f = if g == Female then 1 else 0
        j = if s == Junior then 1 else 0
        l = if d == Legal then 1 else 0
        t = if d == Trading then 1 else 0
    in (eid, Actor $ listVector 3
        [ 1, f, j])

sendIntervals :: Intervals
sendIntervals = Intervals.fromList $
    map (realToFrac . (3600*) . (2^^)) $    
        [ 0 ]

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (realToFrac . (3600*) . (2^^)) $
        [ -1..0 ]
-}

getLogLik :: (IConnection conn) => conn -> Model -> IO (LogLik)
getLogLik conn m = do
    tms <- map fromEmail `fmap` fetchEmailList conn
    let t0 = (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        mhs = [ (msg,h) | (_,msg,h) <- tmhs ]
    return $! LogLik.fromMessages m mhs

optWithPenalty :: Double
               -> (Vector Double -> (Double, Vector Double, a))
               -> Vector Double
               -> (Double, Vector Double, a)
               -> Either (BFGS.Warning, BFGS.Result a)
                         (BFGS.Result a)
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
                                          , LineSearch.valueTol = 1e-2
                                          , LineSearch.derivTol = 0.1
                                          , LineSearch.stepMin = 1e-10
                                      }
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
                     -> Maybe (Double, BFGS.Result a)
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
                           in if (diff < 1e-8 * abs (BFGS.resultValue r)
                                      || diff < 1e-16)
                                  then Just (penalty0, r)
                                  else next
                               

whichMedianVector :: (Ord a, Storable a) => Vector a -> Int
whichMedianVector x = let
    n = dimVector x
    xs = elemsVector x
    in snd $ sort (zip xs [ 0.. ]) !! (n `div` 2)

main :: IO ()        
main = do
    conn <- connectSqlite3 "enron.db"
    ss <- (Map.fromList . map (fromEmployee True)) `fmap` fetchEmployeeList conn
    rs <- ss `seq` (Map.fromList . map (fromEmployee False)) `fmap` fetchEmployeeList conn    
    tms <- rs `seq` (map fromEmail `fmap` fetchEmailList conn)
    let v  = Vars.fromActors ss rs sendIntervals receiveIntervals
        t0 = (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        mhs = [ (msg,h) | (_,msg,h) <- tmhs ]
        
        smry = Summary.fromMessages v mhs
    
        dyn_dim = ( Intervals.size sendIntervals
                  + Intervals.size receiveIntervals )
        send_dim = dimVector $ snd $ Map.findMin ss
        recv_dim = dimVector $ snd $ Map.findMin rs        
        stat_dim = send_dim * recv_dim
        stat_sums = matrixViewVector
                        (send_dim,recv_dim)
                        (dropVector dyn_dim $ Summary.varsSum smry)
        baseline = [ ( dyn_dim
                     + i
                     + send_dim * whichMedianVector r
                     )
                   | (i,r) <- zip [ 0..] (rowsMatrix stat_sums)
                   ]
        na = [ i | (i,c) <- assocsVector $ Summary.varsSum smry, c == 0 ]
        fixed = nub $ baseline ++ na
        notfixed = [ 0..Vars.dim v - 1 ] \\ fixed
        
        beta0 = replaceVector (constantVector (Vars.dim v) 0)
                              [ (i,-10000) | i <- na ]
                            
        xdim = length notfixed
        x0 = constantVector xdim (0::Double)
        fdf x = let
            beta = replaceVector beta0 $ zip notfixed (elemsVector x)
            m = Model.fromVars v beta Model.NoLoops
            ll = LogLik.fromMessages m mhs
            (val,grad) = LogLik.valueGrad ll
            gradx = listVector xdim [ atVector grad i | i <- notfixed ]
            scale = 1 -- 0.5 * (fromIntegral $ LogLik.residDf ll)
            in ( -(val/scale)
               , scaleVector (-1/scale) gradx
               , ll
               )
        
        penalty0 = 1
        opt = optWithPenaltyShrink 0.1 penalty0 fdf x0 (fdf x0)
        
    putStrLn $ "Observed Vars Sum: "
             ++ show (elemsVector $ Summary.varsSum smry)
             
    -- putStrLn $ "Observed Counts: "
    --         ++ show (Summary.counts smry)
    
    putStrLn ""

    case opt of
        Nothing ->
            putStrLn $ "Minimization algorithm failed to converge: "

        Just (penalty,r) -> let
            ll = BFGS.resultState r
            in do
                putStrLn $ "Null Deviance: " ++ show (LogLik.nullDeviance ll)
                putStrLn $ "Null Df: " ++ show (LogLik.nullDf ll)    

                putStrLn $ "Deviance: " ++ show (LogLik.deviance ll)
                putStrLn $ "Resid. Df: " ++ show (LogLik.residDf ll)
                
                putStrLn $ "coefs: " ++ show (elemsVector $ BFGS.resultPos r)

    disconnect conn

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main
    where

import Data.List( foldl' )
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
        
import Numeric.LinearAlgebra
       
        
import Enron
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
import qualified Vars as Vars

     
fromEmail :: Email -> (Time, Message)
fromEmail (Email _ _ time _ f ts) =
    (time, Message f ts)

fromEmployee :: Bool -> Employee -> (ActorId, Vector Double)
fromEmployee intercept (Employee eid _ _ _ g s d) =
    let f = if g == Female then 1 else 0
        j = if s == Junior then 1 else 0
        l = if d == Legal then 1 else 0
        t = if d == Trading then 1 else 0
        xs = withInt [ f, j, l, t, f*j, f*l, f*t, j*l, j*t, f*j*l, f*j*t ]
        p = length xs
    in (eid, listVector p xs)
  where
    withInt = if intercept then (1:) else id

sendIntervals :: Intervals
sendIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $    
        [ -7..2 ] ++ [ 4..13 ]

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $
        [ -7..11 ]

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


main :: IO ()        
main = do
    conn <- connectSqlite3 "enron.db"
    ss <- (Map.fromList . map (fromEmployee False)) `fmap` fetchEmployeeList conn
    rs <- ss `seq` (Map.fromList . map (fromEmployee True)) `fmap` fetchEmployeeList conn    
    tms <- rs `seq` (map fromEmail `fmap` fetchEmailList conn)
    let v  = Vars.fromActors ss rs sendIntervals receiveIntervals
        t0 = (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        mhs = [ (msg,h) | (_,msg,h) <- tmhs ]
        
        beta0 = constantVector (Vars.dim v) 0
        m0 = Model.fromVars v beta0 Model.NoLoops
        
        penalty = 1e-4
        
        nll beta = let m = Model.fromVars v beta Model.NoLoops
                       ll = LogLik.fromMessages m mhs
                       (f,g) = LogLik.valueGrad ll
                       df = fromIntegral $ LogLik.residDf ll
                   in ( -f + 0.5 * penalty * (beta `dotVector` beta)
                      , addVectorWithScales (-1) g penalty beta
                      , ll
                      )
    
        opt = BFGS.minimize BFGS.defaultControl{
                                  BFGS.linesearchControl =
                                      LineSearch.defaultControl{
                                          LineSearch.verbose = True
                                      }
                                , BFGS.gradTol = 1e-5
                                , BFGS.verbose = True }
                          nll beta0 (nll beta0)
    
    case opt of
        Left (w,r) -> do
            putStrLn $ "Minimization algorithm failed to converge: "
                     ++ show w
            putStrLn $ "Gradient norm: " ++ show (norm2Vector $ BFGS.resultGrad r)
            let ll = BFGS.resultState r
            putStrLn $ "Null Deviance: " ++ show (LogLik.nullDeviance ll)
            putStrLn $ "Null Df: " ++ show (LogLik.nullDf ll)    

            putStrLn $ "Deviance: " ++ show (LogLik.deviance ll)
            putStrLn $ "Resid. Df: " ++ show (LogLik.residDf ll)    

        Right r -> let
            ll = BFGS.resultState r
            in do
                putStrLn $ "Null Deviance: " ++ show (LogLik.nullDeviance ll)
                putStrLn $ "Null Df: " ++ show (LogLik.nullDf ll)    

                putStrLn $ "Deviance: " ++ show (LogLik.deviance ll)
                putStrLn $ "Resid. Df: " ++ show (LogLik.residDf ll)    
    
    disconnect conn

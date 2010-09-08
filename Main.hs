{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main
    where

import Debug.Trace( trace )
import Data.List( foldl' )
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

optWithPenalty :: Double
               -> Model
               -> [(Message, History)]
               -> Either (BFGS.Warning, BFGS.Result LogLik)
                         (BFGS.Result LogLik)
optWithPenalty penalty m0 mhs = let
    beta0 = Model.coefs m0
    loops = if Model.hasLoops m0 then Model.Loops else Model.NoLoops
    nll beta = let m = Model.fromVars (Model.vars m0) beta loops
                   ll = LogLik.fromMessages m mhs
                   (f,g) = LogLik.valueGrad ll
                   df = fromIntegral $ LogLik.residDf ll
               in ( -2*f/df + (penalty/df) * (beta `dotVector` beta)
                  , addVectorWithScales (-2/df) g (2*penalty/df) beta
                  , ll
                  )
    
    opt = BFGS.minimize BFGS.defaultControl{
                                BFGS.linesearchControl =
                                      LineSearch.defaultControl{
                                            LineSearch.verbose = True
                                          , LineSearch.valueTol = 0.01
                                          , LineSearch.derivTol = 0.1
                                      }
                                , BFGS.iterMax = 10000
                                , BFGS.gradTol = 1e-5
                                , BFGS.verbose = True }
                          nll beta0 (nll beta0)
    
    in opt

optWithPenaltyShrink :: Double
                     -> Double
                     -> Model
                     -> [(Message, History)]
                     -> Maybe (Double, BFGS.Result LogLik)
optWithPenaltyShrink = go Nothing
  where
    go prev shrink penalty0 m0 mhs =
        case (trace ("\n\npenalty = " ++ show penalty0 ++ "\n\n")
                    optWithPenalty penalty0 m0 mhs) of
            Left _   -> prev
            Right r0 -> 
                trace ("\n\ndeviance = " ++ show (LogLik.deviance $ BFGS.resultState r0)
                      ++ "\n\ncoefs = " ++ show (Model.coefs $ LogLik.model $ BFGS.resultState r0)
                      ++ "\n\n"
                      )
                go (Just (penalty0, r0))
                   shrink
                   (shrink*penalty0)
                   (LogLik.model $ BFGS.resultState r0)
                   mhs

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
        
        beta0 = constantVector (Vars.dim v) 0
        m0 = Model.fromVars v beta0 Model.NoLoops
        penalty0 = 1e5
        opt = optWithPenaltyShrink 0.5 penalty0 m0 mhs
    
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

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
import Types

import qualified Intervals as Intervals
import qualified History as History
import qualified Model as Model
import qualified LogLik as LogLik
import qualified Vars as Vars

     
fromEmail :: Email -> (Time, Message)
fromEmail (Email _ _ time _ f ts) =
    (time, Message f ts)

fromEmployee :: Employee -> (ActorId, Vector Double)
fromEmployee (Employee eid _ _ _ g s d) =
    let f = if g == Female then 1 else 0
        j = if s == Junior then 1 else 0
        l = if d == Legal then 1 else 0
        t = if d == Trading then 1 else 0
    in (eid, 
        listVector 12
            [ 1, f, j, l, t, f*j, f*l, f*t, j*l, j*t, f*j*l, f*j*t ])

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

main :: IO ()        
main = do
    conn <- connectSqlite3 "enron.db"
    as <- (Map.fromList . map fromEmployee) `fmap` fetchEmployeeList conn
    tms <- as `seq` (map fromEmail `fmap` fetchEmailList conn)
    let v = Vars.fromActors as as sendIntervals receiveIntervals
        t0 = if null tms then posixSecondsToTime 0
                         else (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        mhs = [ (msg,h) | (_,msg,h) <- tmhs ]
        m = Model.fromVars v (constantVector (Vars.dim v) 0) Model.NoLoops
        ll = LogLik.fromMessages m mhs
        
    putStrLn $ "Null Deviance: " ++ show (LogLik.nullDeviance ll)
    putStrLn $ "Null Df: " ++ show (LogLik.nullDf ll)    

    putStrLn $ "Deviance: " ++ show (LogLik.deviance ll)
    putStrLn $ "Resid. Df: " ++ show (LogLik.residDf ll)    
    
    -- let (score,fisher) = LogLik.fisherWithScore ll
    -- putStrLn $ "Score: \n" ++ show score
    -- putStrLn $ "Fisher: \n" ++ show fisher
    
    disconnect conn

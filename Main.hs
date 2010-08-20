module Main
    where

import Data.List( foldl', mapAccumL )
import Data.Map( Map )
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
        
import Data.Time
import Data.Time.Clock.POSIX
import Numeric.LinearAlgebra
       
        
import Actor
import qualified History as History
import qualified DVars as DVars
import Enron
import Intervals( Intervals )
import qualified Intervals as Intervals
import qualified LogLik as LogLik
import Message
import Params( defaultParams )
import qualified SVars as SVars
import qualified Summary as Summary

     
fromEmail :: Email -> (UTCTime, Message)
fromEmail (Email _ _ time _ f ts) =
    (time, Message f ts)

fromEmployee :: Employee -> (ActorId, Actor)
fromEmployee (Employee eid _ _ _ g s d) =
    let f = if g == Female then 1 else 0
        j = if s == Junior then 1 else 0
        l = if d == Legal then 1 else 0
        t = if d == Trading then 1 else 0
    in (eid, Actor $ listVector 12
        [ 1, f, j, l, t, f*j, f*l, f*t, j*l, j*t, f*j*l, f*j*t ])

sendIntervals :: Intervals
sendIntervals = Intervals.fromList $
    map (realToFrac . (3600*) . (2^^)) $    
        [ -7..2 ] ++ [ 4..13 ]

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (realToFrac . (3600*) . (2^^)) $
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

        
main = do
    conn <- connectSqlite3 "enron.db"
    as <- (Map.fromList . map fromEmployee) `fmap` fetchEmployeeList' conn
    tms <- map fromEmail `fmap` fetchEmailList' conn
    let sv = SVars.fromActors as as
        dv = DVars.fromIntervals sendIntervals receiveIntervals 
        t0 = if null tms then posixSecondsToUTCTime 0
                         else (fst . head) tms
        h0 = DVars.history t0 dv
        cms = snd $ History.accum h0 tms
        --smry = Summary.fromList sv dv cms
        p = defaultParams sv dv
        (ll,rs) = mapAccumL (flip LogLik.insert) (LogLik.empty p) cms
        
    -- putStrLn $ "message count: " ++ show (Summary.messageCount smry)
    -- putStrLn $ "summary: " ++ show smry

    putStrLn $ "Null Deviance: " ++ show (LogLik.nullDeviance ll)
    putStrLn $ "Null Df: " ++ show (LogLik.nullDf ll)    

    putStrLn $ "Deviance: " ++ show (LogLik.deviance ll)
    putStrLn $ "Resid. Df: " ++ show (LogLik.residDf ll)    
    
    let (score,fisher) = LogLik.fisherWithScore ll
    putStrLn $ "Score: \n" ++ show score
    putStrLn $ "Fisher: \n" ++ show fisher
    
    disconnect conn

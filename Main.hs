module Main
    where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
        
import Data.Time
import Data.Time.Clock.POSIX
import Numeric.LinearAlgebra
       
        
import Actor
import Intervals( Intervals )
import qualified Intervals as Intervals
import qualified DVars as DVars
import qualified SVars as SVars
import Params( defaultParams )
import Enron
import Message
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

recvIntervals :: Intervals
recvIntervals = Intervals.fromList $
    map (realToFrac . (3600*) . (2^^)) $
        [ -7..11 ]
        
main = do
    conn <- connectSqlite3 "enron.db"
    as <- (Map.fromList . map fromEmployee) `fmap` fetchEmployeeList' conn
    tms <- map fromEmail `fmap` fetchEmailList' conn
    let sv = SVars.fromActors as as
        t0 = if null tms then posixSecondsToUTCTime 0
                         else (fst . head) tms
        dv0 = DVars.empty sendIntervals recvIntervals t0
        mds = snd $ DVars.accum dv0 tms
        smry = Summary.fromList sv mds
        p = defaultParams sv dv0
        -- dev = foldl' updateDeviance (emptyDeviance sv dv p) $ zip ms hs
        
    putStrLn $ "message count: " ++ show (Summary.messageCount smry)
    putStrLn $ "summary: " ++ show smry
    -- putStrLn $ "deviance: " ++ show (value dev)
    
    disconnect conn
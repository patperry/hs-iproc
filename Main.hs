module Main
    where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
        
import Data.Time
        
--import Actor
import qualified IntervalSet as IntervalSet
import qualified DVars as DVars
import qualified SVars as SVars
import Params( defaultParams )
import Enron
import Message
import qualified Summary as Summary
        

sendIntervals = IntervalSet.fromList $
    map (realToFrac . floor . (3600*) . (2^^)) $    
        [ -7..2 ] ++ [ 4..13 ]

recvIntervals = IntervalSet.fromList $
    map (realToFrac . floor . (3600*) . (2^^)) $
        [ -7..11 ]
        
main = do
    conn <- connectSqlite3 "enron.db"
    as <- (Map.fromList . map fromEmployee) `fmap` fetchEmployeeList' conn
    ms <- map fromEmail `fmap` fetchEmailList' conn
    let sv = SVars.fromActors as as
        t0 = messageTime $ head ms
        dv0 = DVars.empty sendIntervals recvIntervals t0
        mds = snd $ Message.accumDVars dv0 ms
        smry = Summary.fromList sv mds
        p = defaultParams sv dv0
        -- dev = foldl' updateDeviance (emptyDeviance sv dv p) $ zip ms hs
        
    putStrLn $ "message count: " ++ show (Summary.messageCount smry)
    putStrLn $ "summary: " ++ show smry
    -- putStrLn $ "deviance: " ++ show (value dev)
    
    disconnect conn
module Main
    where

import Data.List( foldl' )
import Database.HDBC
import Database.HDBC.Sqlite3
        
import Numeric.LinearAlgebra
        
import Actor
import DVars
import SVars
import Param
import Deviance
import Enron
import Message
import Summary
        
sendIntervals :: Vector DiffTime
sendIntervals =
    listVector 20 $ map (floor . (3600*) . (2^^)) $
        [ -7..2 ] ++ [ 4..13 ]

recvIntervals :: Vector DiffTime
recvIntervals =
    listVector 19 $ map (floor . (3600*) . (2^^)) $
        [ -7..11 ]

        
main = do
    conn <- connectSqlite3 "enron.db"
    as <- map actorFromEmployee `fmap` fetchEmployeeList' conn
    ms <- map messageFromEmail `fmap` fetchEmailList' conn
    let sv = svarsWith allDyadVars as as
        dv = dvarsWithIntervals sendIntervals recvIntervals
        t0 = 0
        h0 = emptyDVarsState t0 dv
        hs = snd $ messageHistory h0 ms
        summ = foldl' updateSummary (emptySummary sv dv) $ zip ms hs
        p = defaultParam (actorSet as) (actorSet as) sv dv
        dev = foldl' updateDeviance (emptyDeviance sv dv p)
        
    putStrLn $ "message count: " ++ show (messageCount summ)
    disconnect conn
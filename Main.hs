module Main
    where

import Database.HDBC
import Database.HDBC.Sqlite3
        
import Numeric.LinearAlgebra
        
import DVars
import SVars
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
    let svars = svarsWith allDyadVars as as
        dvars = dvarsWithIntervals sendIntervals recvIntervals
        t0 = 0
        summ = listSummary svars dvars t0 ms
    putStrLn $ "message count: " ++ show (messageCount summ)
    disconnect conn
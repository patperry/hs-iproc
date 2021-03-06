{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main
    where

import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Matrix as M
import qualified Numeric.LinearAlgebra.Vector as V

import System.IO( stdout ) 
        
import Enron
import History( History )
import Intervals( Intervals )
import LogLik( LogLik )
import Model( Model )
import Types

import qualified Fit as Fit
import qualified FitSummary as FitSummary
import qualified Intervals as Intervals
import qualified Fisher as Fisher
import qualified History as History
import qualified Model as Model
import qualified LogLik as LogLik
import qualified Vars as Vars

     
messageFromEmail :: Email -> (Time, Message)
messageFromEmail (Email _ _ time _ f ts) =
    (time, Message f ts)

senderFromEmployee :: Employee -> (SenderId, Vector Double)
senderFromEmployee = actorFromEmployee

receiverFromEmployee :: Employee -> (ReceiverId, Vector Double)
receiverFromEmployee = actorFromEmployee

actorFromEmployee :: Employee -> (ActorId, Vector Double)
actorFromEmployee (Employee eid _ _ _ gen sen dep) =
    let f = if gen == Female then 1 else 0
        m = if gen == Male then 1 else 0
        j = if sen == Junior then 1 else 0
        s = if sen == Senior then 1 else 0
        l = if dep == Legal then 1 else 0
        t = if dep == Trading then 1 else 0
        o = if dep == Other then 1 else 0
        xs = [ f*l*j, f*l*s, f*t*j, f*t*s, f*o*j, f*o*s
             , m*l*j, m*l*s, m*t*j, m*t*s, m*o*j, m*o*s
             ]
        p = length xs
    in (eid, V.fromList p xs)

sendIntervals :: Intervals
sendIntervals = Intervals.fromList []

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $
        [ -6..14 ]

maxRecipCount :: Int
maxRecipCount = 10

main :: IO ()        
main = do
    conn <- connectSqlite3 "enron.db"
    ss <- (Map.fromList . map senderFromEmployee) `fmap` fetchEmployeeList conn
    rs <- ss `seq` (Map.fromList . map receiverFromEmployee) `fmap` fetchEmployeeList conn    
    tms0 <- rs `seq` (map messageFromEmail `fmap` fetchEmailList conn)
    let tms = [ (t,msg) | (t,msg) <- tms0
                        , length (messageTo msg) <= maxRecipCount ]
        v  = Vars.fromActors ss rs sendIntervals receiveIntervals
        t0 = (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        mhs = [ (msg,h) | (_,msg,h) <- tmhs ]
        
        beta0 = V.constant (Vars.dim v) 0
        fixed = []
        m0 = Model.fromVars v beta0 Model.NoLoops                          
        fit = Fit.fromMessagesWithFixed fixed m0 mhs
    
    case fit of
        Nothing ->
            putStrLn $ "Minimization algorithm failed to converge: "

        Just (penalty,r) -> let
            ll = Fit.resultState r
            fish = Fisher.fromMessages (LogLik.model ll) mhs
            in do
                FitSummary.hPutFitSummary stdout (penalty,r)
                FitSummary.hPutCovSummary stdout fish

    disconnect conn

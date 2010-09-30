{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main
    where

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

import qualified Fit as Fit
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
        xs = [ f*s*l, m*s*l, f*s*t, m*s*t, f*s*o, m*s*o
             , f*j*l, m*j*l, f*j*t, m*j*t, f*j*o, m*j*o
             ]
        p = length xs
    in (eid, listVector p xs)

sendIntervals :: Intervals
sendIntervals = Intervals.fromList []

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $
        [ -6..14 ]

main :: IO ()        
main = do
    conn <- connectSqlite3 "enron.db"
    ss <- (Map.fromList . map senderFromEmployee) `fmap` fetchEmployeeList conn
    rs <- ss `seq` (Map.fromList . map receiverFromEmployee) `fmap` fetchEmployeeList conn    
    tms <- rs `seq` (map messageFromEmail `fmap` fetchEmailList conn)
    let v  = Vars.fromActors ss rs sendIntervals receiveIntervals
        t0 = (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        mhs = [ (msg,h) | (_,msg,h) <- tmhs ]
        
        beta0 = constantVector (Vars.dim v) 0
        fixed = []
        m0 = Model.fromVars v beta0 Model.NoLoops                          
        fit = Fit.fromMessagesWithFixed fixed m0 mhs
    
    case fit of
        Nothing ->
            putStrLn $ "Minimization algorithm failed to converge: "

        Just (penalty,r) -> let
            ll = Fit.resultState r
            fish = Fisher.fromMessages (LogLik.model ll) mhs
            tol = 1e-6
            (fishinv,nzero) = Fisher.invWithTol tol fish
            stderr = withHerm fishinv $ \_ a -> sqrtVector $ diagMatrix a
            in do
                putStrLn $ "Null Deviance: " ++ show (LogLik.nullDeviance ll)
                putStrLn $ "Null Df: " ++ show (LogLik.nullDf ll)    

                putStrLn $ "Deviance: " ++ show (LogLik.deviance ll)
                putStrLn $ "Resid. Df: " ++ show (LogLik.residDf ll)
                
                putStrLn $ "rank of Fisher matrix: "
                         ++ show (Vars.dim v - nzero)
                
                putStrLn $ "coefs: "
                         ++ show (elemsVector $ Model.coefs $ LogLik.model ll)
                         
                putStrLn $ "stderr: "
                         ++ show (elemsVector stderr)

    disconnect conn

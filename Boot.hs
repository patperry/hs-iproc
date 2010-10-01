{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main
    where

import qualified Data.Map as Map
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad.MC( evalMC, mt19937 )
import Numeric.LinearAlgebra
import System( getArgs )
import System.IO( stdout, Handle, hPutStrLn ) 
        
import Enron
import History( History )
import Intervals( Intervals )
import LogLik( LogLik )
import Model( Model )
import Types

import qualified Bootstrap as Bootstrap
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
    in (eid, listVector p xs)

sendIntervals :: Intervals
sendIntervals = Intervals.fromList []

receiveIntervals :: Intervals
receiveIntervals = Intervals.fromList $
    map (fromIntegral . floor . (3600*) . (2^^)) $
        [ -6..14 ]

coefs :: Vector Double
coefs = listVector 165 [7.374712552037485, 6.126792616537681, 6.294680871527387, 5.917602266049506, 5.570867214035314, 5.304553821851981, 5.155626634083695, 4.945613746223219, 4.689000321177603, 4.527039443599424, 4.573181762775575, 4.395457852121, 4.195781296001477, 4.115466026466157, 3.8563493978843995, 3.5775394174998993, 3.275104012160893, 2.929234531896255, 2.489571399002263, 2.132195045538634, 1.7952136696516796, 1.0921810550158582, 1.1233309570055197, 0.43923377586143914, 0.34956378768341806, -1.229765418093693, -0.5182616457293765, 0.9031435250048878, 1.1239793073724298, -1.2360773551842574, -2.4050192860795656, -2.0669226497267887, -0.7676494973886046, 0.901931816619991, 1.4282220293543415, -1.569511682960474, -0.32606514874145237, -1.1127753046779951, -0.10125278855633535, -0.7414690508917862, 0.8846437013070172, 0.9791498569120312, -1.0009005046289625, -1.9742060837757935, -1.3681269952697572, -0.8388887946995266, -0.49202046225141355, 0.48872263658863047, 0.36656243400090127, -0.5000254842114875, -0.14895296911802264, 0.8913801205289158, -2.5175447966527695, 1.773608664212337, 0.7540518814751208, 1.195891453594171, -0.8069241836917651, -3.622778326618091e-2, -0.7754406952203509, 1.7740349527290942, 1.1659489873867228, 0.7728685334482654, -1.2876078702316762, -0.828295348593001, -0.7854065078630723, 0.2349621972318447, 0.7356354556147782, -0.254857595827706, -0.2719441743058683, -0.7506458170986358, -2.1088330328059217, -0.3873113603190351, 0.42500906010413986, 1.021333961616508, 0.5200479955087102, -0.9009480574890791, -1.440263142682427, -1.3494742803765438, 0.4484420574436636, 0.6549336579053359, 0.8849033658035927, -3.349949055784765e-2, -0.25299206816796266, -2.077060858930669, -0.7075282121648196, 0.34106165198073884, 1.1108789519621831, 0.5783888735500956, 0.8007972735008634, -2.2570798271963044, -0.10300616298407315, 0.5349458297816317, 0.46831673283433206, 0.4278035296009331, -8.084042136476478e-2, 1.39842687200985, -0.3085537278019918, 0.3474405426564546, -0.39527721676347094, -0.1364712793886758, 0.5541277888401204, -0.335025052687693, 0.9402367457190062, -1.608576687356857e-2, 0.3925112440814065, 0.9527536911343426, 1.306913782117101, -0.26761222019786984, -0.3616554661108776, -1.0728688954390009, 0.21382315630626272, 0.41875407930433317, 1.7107993867298268, -0.5969545348856147, -0.5129824839032868, -1.1893130540644152, 0.38547125311462377, -0.34129409849711645, 0.5123616679554251, 0.599137232127625, -0.5035688517290905, -0.35228459095228115, -0.5886472617751958, -0.4706330895384878, 0.2868897629453806, 0.8298529206214352, -4.59443876163528e-2, 0.9928872061615378, -0.15584803206807882, -0.4328612034419299, 0.632806616550431, 1.0226504685372706, 0.8549487518276475, 0.44226694862892196, 0.42039576040248433, 1.0680739589051973, -0.17547539905327764, 1.0919868786396751, 0.855156650735699, 1.0002406268269226, 0.6800163968676748, -0.7737382348879057, -1.296703867335525, 0.735409278838224, 0.3851282347090524, 0.5621078365532806, -0.25850336319573997, -0.45460776252281443, -2.034491049551125, 1.2962531016075431, 0.6476035952606838, 1.359480298213726, -0.27485634465707154, -0.16751466992187705, 3.195494163009414e-3, -2.156119094284088, -1.3397898491636522, 0.7806402184902745, 1.0333572511902251, -0.32731596886959624, 1.5919436751069445, -0.431202568894527, -0.3132735610367038, -0.23699392221497842, 0.8341302346795448]

main :: IO ()        
main = do
    args <- getArgs
    let seed = if null args then 0
                            else (read . head) args
    conn <- connectSqlite3 "enron.db"
    ss <- (Map.fromList . map senderFromEmployee) `fmap` fetchEmployeeList conn
    rs <- ss `seq` (Map.fromList . map receiverFromEmployee) `fmap` fetchEmployeeList conn    
    tms <- rs `seq` (map messageFromEmail `fmap` fetchEmailList conn)
    let v  = Vars.fromActors ss rs sendIntervals receiveIntervals
        t0 = (fst . head) tms
        h0 = History.empty
        tmhs = snd $ History.accum (t0,h0) tms
        slhs = [ ((messageFrom msg, length (messageTo msg)),h)
               | (_,msg,h) <- tmhs ]
        model = Model.fromVars v coefs Model.NoLoops                          
        (lw,mhs) = Bootstrap.fromSends model slhs `evalMC` mt19937 seed
        

        beta0 = constantVector (Vars.dim v) 0
        fixed = []
        m0 = Model.fromVars v beta0 Model.NoLoops
        fit = Fit.fromMessagesWithFixed fixed m0 mhs
    
    case fit of
        Nothing ->
            putStrLn $ "Minimization algorithm failed to converge: "

        Just (penalty,r) -> let
            ll = Fit.resultState r
            in do
                hPutScalar stdout "seed" seed
                hPutScalar stdout "log.wt" lw
                FitSummary.hPutFitSummary stdout (penalty,r)
                
    disconnect conn


hPutScalar :: (Show a) => Handle -> String -> a -> IO ()
hPutScalar h name a = do
    hPutStrLn h $ name ++ " <- " ++ show a

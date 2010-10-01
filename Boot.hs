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

maxRecipCount :: Int
maxRecipCount = 10

coefs :: Vector Double
coefs = listVector 165 [7.395942767750519, 6.52487816112583, 6.43843224486956, 6.013799863995172, 5.678303347349773, 5.440780457223448, 5.315083774871851, 5.124078171345275, 4.824780231741018, 4.695717041112421, 4.737358229704321, 4.5415367362001735, 4.349772530879135, 4.260880284510069, 4.002999477470879, 3.720715384292371, 3.3979282981512626, 3.057188513031487, 2.568019462754001, 2.1296354436820666, 1.64954384703688, 1.0474732386909646, 1.0738021272378597, 0.41545928378488883, 0.2917907698326899, -1.168985736237329, -0.30018167320373346, 0.8768695665586259, 1.0772831415835211, -1.268458383736621, -2.450958009351548, -2.0892662726620053, -0.36944302212640046, 0.8472356294314468, 1.3628643201562805, -1.5251921401243627, -0.32319258671155343, -1.0238337930915, 1.6462781125040946e-2, -0.7967473784951865, 0.9191797519937458, 0.874473063536432, -0.8366947038602779, -2.0869770589330336, -0.9462616455566688, -0.7882643870771263, -0.5262325960662194, 0.28129779005982447, 0.33065890994917546, -0.7470071081430316, -6.903625518473551e-2, 0.849476693489796, -2.62288011752259, 1.765573396861904, 0.8030887580962879, 1.1014986373795195, -2.420028286568595, -0.17654142794947764, -0.5961185473078469, 1.3826758119446307, 1.0082070559707, 0.7748863208352216, -1.7165706774486733, -0.8785768283198777, -0.6876376775703688, 0.15765328686616353, 0.661000871877854, -0.5585768482029765, -1.1383635476866683, -0.7013625877822367, -2.014086580061167, -0.16464904986200196, 0.4854318957376308, 1.0108873819278645, 0.6746476189926561, -0.8987585843531877, -1.3230427673014677, -1.2971969864791681, 3.8757353342293347e-3, 0.7146650709026696, 1.1860713773540927, 4.7041541839372335e-2, 0.13935699129939377, -1.9577122356388177, -0.4447034299950774, 0.3920006725578448, 1.290231840044372, 0.5140129961328167, 0.869064048979677, -2.106527727876332, 0.4992540293215699, 0.7395272993226445, 0.8095990119256652, 0.34815481336354415, 1.2698142257056306e-2, 1.296312792007703, -0.3842971008452565, 0.38122923828868926, -0.1819232184638107, -0.12595508523478607, 0.41220072716067285, -0.3555670854382109, 0.8113978543704727, 5.032435919862131e-2, 0.7831355208607707, 0.9800122631086299, 1.3107952586254719, -0.11740577505905844, -0.46960750892000047, -1.0124242811341095, 0.4101651899883326, 0.6026564690510375, 1.7450361185404777, -0.6416395778282624, -0.24537769242289229, -1.1272951210598332, 0.7712564487282079, -0.3292670386743983, 0.4109773418331828, 0.6635162459977982, -0.4897243187472064, -0.4307228704231112, -1.138410030713018, -0.4418083138842616, 0.3036989169474513, 0.8531169747244779, -1.255549378225712e-2, 0.9762947573786525, -0.5695940391293699, -0.4510520757721083, 0.2922669140497551, 0.9401087304686275, 0.7957584703801924, 0.4557999484425962, -1.0223019447341579e-3, 1.0657631661844373, -0.4146900404919057, 1.062625032520594, 0.756084304399564, 0.9570092668700597, 0.789845289591196, -0.7124266224348069, -1.2389066128342134, 0.7029240884570925, 0.3210686378315024, 0.5053392021092676, -0.24121613358878335, -0.42239052137019345, -1.9255472313886504, 1.2738498424591624, 0.16296392764566336, 1.3292804268949192, -0.12503380471184097, -0.11100334674344735, -0.2274167591896137, -1.9173355420363647, -1.1213907944828139, 0.8628310248675123, 1.2568528603970508, -0.34454217975923973, 1.6473351290692093, -0.3179018356101458, -0.15207958162880358, -6.484517089392538e-3, 1.2288166973195993]

main :: IO ()        
main = do
    args <- getArgs
    let seed = if null args then 0
                            else (read . head) args
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

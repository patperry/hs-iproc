module LogLik (
    LogLik,
    fromMessages,
    model,
    
    deviance,
    nullDeviance,   
    nullDf,
    residDf,
    
    score,
    devianceScore,
    
    value,
    grad,
    valueGrad,
    
    ) where

import Debug.Trace( trace )
import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import History( History )
import Model( Model )
import Types( ReceiverId, SenderId, Message(..) )
import qualified History as History
import qualified Model as Model
import qualified Vars as Vars


unionWith' :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith' f m m' =
    foldl' (flip $ uncurry $ Map.insertWith' (flip f))
           m
           (Map.toList m')
           
           
           
data SenderLogLik =
    SenderLogLik { senderModel :: !Model
                 , sender :: !SenderId
                 , sendCount :: !Int
                 , receiveCount :: !(Map ReceiverId Int)
                 , observedVarChanges :: !(Map Int Double)
                 , valueSLL :: !Double
                 , sumInvWeight :: !Double
                 , sumInvWeightLogScale :: !Double
                 , receiverWeights :: !(Map ReceiverId Double)
                 , expectedVarChanges :: !(Map Int Double)
                 }
    deriving (Show)

observedVarsSLL :: SenderLogLik -> Vector Double
observedVarsSLL sll = let
    rws = [ (r, fromIntegral w) | (r,w) <- Map.assocs rc ]
    in accumVector (+) (Vars.weightReceiverBy rws v h0 s) (Map.assocs ovc)
  where
    m = senderModel sll
    v = Model.vars m
    s = sender sll
    rc = receiveCount sll
    ovc = observedVarChanges sll
    h0 = History.empty

expectedVarsSLL :: SenderLogLik -> Vector Double
expectedVarsSLL sll = let
    rws = expectedReceiverCountsSLL sll
    in accumVector (+) (Vars.weightReceiverBy rws v h0 s) (Map.assocs evc)
  where
    m = senderModel sll
    v = Model.vars m
    s = sender sll
    evc = expectedVarChanges sll
    h0 = History.empty

expectedReceiverCountsSLL :: SenderLogLik -> [(ReceiverId, Double)]
expectedReceiverCountsSLL sll =
    [ (r, exp (Model.logProb m h0 s r + scale)
          + Map.findWithDefault 0 r rws)
    | r <- Model.validReceivers m s
    ]
  where
    m = senderModel sll
    s = sender sll
    scale = log (sumInvWeight sll) - sumInvWeightLogScale sll
    rws = receiverWeights sll
    h0 = History.empty

valueGradSLL :: SenderLogLik -> (Double, Vector Double)
valueGradSLL sll =
    let x = observedVarsSLL sll
        mu = expectedVarsSLL sll
        f = valueSLL sll
        g = x `subVector` mu
    in if any isNaN (elemsVector g)
            then error ("NaN Grad\n  obs: " ++ show (elemsVector x)
                        ++ "\n  exp: " ++ show (elemsVector mu))
            else (f, g)

unionSLL :: SenderLogLik -> SenderLogLik -> SenderLogLik
unionSLL (SenderLogLik m s sc1 rc1 ovc1 v1 siw1 logscale1 rw1 evc1)
         (SenderLogLik _ _ sc2 rc2 ovc2 v2 siw2 logscale2 rw2 evc2) = let
    (siw, logscale) = if logscale1 <= logscale2
                          then ( siw1 + siw2 * exp (logscale1 - logscale2)
                               , logscale1 )
                          else ( siw1 * exp (logscale2 - logscale1) + siw2 
                               , logscale2 )
    in SenderLogLik m s
                    ((+) sc1 sc2)
                    (unionWith' (+) rc1 rc2)
                    (unionWith' (+) ovc1 ovc2)
                    ((+) v1 v2)
                    siw logscale
                    (unionWith' (+) rw1 rw2)
                    (unionWith' (+) evc1 evc2)



singletonSLL :: Model -> SenderId -> ([ReceiverId], History) -> SenderLogLik
singletonSLL m s (rs, h) = let
    sc = length rs
    rc = Map.fromList $ zip rs (repeat 1)
    ovc = foldl' (flip $ uncurry $ Map.insertWith' (+)) Map.empty $
              concatMap (Vars.dyadChanges vars h s) rs

    (active, dlws) = unzip [ (r,dlw)
                           | (r,dlw) <- Vars.mulSenderChangesBy beta vars h s
                           , Model.validDyad m s r
                           ]
    logscale = foldl' max 0 dlws
    invscale = exp (-logscale)
    dlws' = [ dlw - logscale | dlw <- dlws ]

    lp0s = map (Model.logProb m h0 s) active
    p0s = map exp lp0s
    lws' = [ lp0 + dlw' | (lp0, dlw') <- zip lp0s dlws' ]
    dws' = [ p0 * (exp dlw' - invscale) | (p0, dlw') <- zip p0s dlws' ]

    sum_w' = invscale + foldl' (+) 0 dws'
    log_sum_w' = log sum_w'
    log_sum_w = log_sum_w' + logscale
    lps = [ lw' - log_sum_w' | lw' <- lws' ]
    
    v = foldl' (+) 0 [ findWithDefault (Model.logProb m h0 s r - log_sum_w)
                                       r
                                       (zip active lps)
                     | r <- rs
                     ]

    siw = fromIntegral sc / sum_w'
    ps = map exp lps
    dps = [ p - exp (lp0 - log_sum_w) | (p,lp0) <- zip ps lp0s ]
    rw = Map.fromList $
             [ (r, fromIntegral sc * dp) | (r,dp) <- zip active dps ]
    evc = Map.fromList $
               Vars.weightReceiverChangesBy
               [ (r, fromIntegral sc * p) | (r,p) <- zip active ps ]
               vars h s
    in SenderLogLik m s sc rc ovc v siw logscale rw evc
  where
    vars = Model.vars m
    beta = Model.coefs m
    h0 = History.empty
    findWithDefault a0 k kas = case lookup k kas of Just a  -> a
                                                    Nothing -> a0


emptySLL :: Model -> SenderId -> SenderLogLik
emptySLL m s = SenderLogLik m s 0 Map.empty Map.empty 0 0 0 Map.empty Map.empty

insertSLL :: ([ReceiverId], History) -> SenderLogLik -> SenderLogLik
insertSLL msg sll =
    unionSLL sll $ singletonSLL (senderModel sll) (sender sll) msg

data LogLik = LogLik { model :: !Model
                     , count :: !Int
                     , senderLogLik :: !(Map SenderId SenderLogLik)
                     }
    deriving (Show)

value :: LogLik -> Double
value = fst . valueGrad

grad :: LogLik -> Vector Double
grad = snd . valueGrad

valueGrad :: LogLik -> (Double, Vector Double)
valueGrad (LogLik m _ sllm) = let
    (fs,gs) = unzip $ map valueGradSLL $ Map.elems sllm
    f = stableSum 0 0 fs -- foldl' (+) 0 fs
    g = sumVector (Vars.dim $ Model.vars m) gs
    in g `seq` f `seq` (f, g)
  where
    stableSum acc err []     = acc + err
    stableSum acc err (x:xs) = let
        x'   = err + x
        acc' = acc + x'
        diff = acc - acc'
        err' = diff + x
        in acc' `seq` err' `seq` stableSum acc' err' xs

deviance :: LogLik -> Double
deviance = fst . devianceScore

score :: LogLik -> Vector Double
score = snd . devianceScore

devianceScore :: LogLik -> (Double, Vector Double)
devianceScore ll = let
    (f, g) = valueGrad ll
    n = fromIntegral $ count ll
    in (-2 * f, scaleVector (1 / sqrt n) g)

nullDeviance :: LogLik -> Double
nullDeviance (LogLik m _ sllm) =
    2 * foldl' (+) 0
        [ ( fromIntegral (sendCount sll)
          * log (fromIntegral $ length $ Model.validReceivers m s)
          )
        | (s,sll) <- Map.assocs sllm
        ]

nullDf :: LogLik -> Int
nullDf ll = count ll

residDf :: LogLik -> Int
residDf ll =
    nullDf ll - Vars.dim (Model.vars $ model ll)

empty :: Model -> LogLik
empty m = LogLik { model = m
                 , count = 0
                 , senderLogLik = Map.empty
                 }

union :: LogLik -> LogLik -> LogLik
union (LogLik m c1 sll1) (LogLik _ c2 sll2) =
    LogLik m (c1 + c2) (unionWith' unionSLL sll1 sll2)

singleton :: Model -> (Message, History) -> LogLik
singleton m (Message s rs, h) =
    LogLik m (length rs) $ Map.singleton s $ singletonSLL m s (rs,h)

insert :: (Message, History) -> LogLik -> LogLik
insert msg ll =
    union ll $ singleton (model ll) msg

fromMessages :: Model -> [(Message, History)] -> LogLik
fromMessages m mhs =
    foldl' (flip LogLik.insert) (LogLik.empty m) mhs

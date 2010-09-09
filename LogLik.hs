{-# LANGUAGE ForeignFunctionInterface #-}
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
import Data.List( foldl', mapAccumL )
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
                 , meanInvWeight :: !Double
                 , meanInvWeightLogScale :: !Double
                 , receiverWeightDiffs :: !(Map ReceiverId Double)
                 , expectedVarChanges :: !(Map Int Double)
                 }
    deriving (Show)

showSLL :: SenderLogLik -> String
showSLL sll =
    (""
    ++ "\nsendCount: " ++ show (sendCount sll)
    ++ "\nreceiveCount: " ++ show (Map.assocs $ receiveCount sll)
    ++ "\nobservedVarChanges: " ++ show (Map.assocs $ observedVarChanges sll)
    ++ "\nvalue: " ++ show (valueSLL sll)
    ++ "\nmeanInvWeight: " ++ show (meanInvWeight sll)
    ++ "\nmeanInvWeightLogScale: " ++ show (meanInvWeightLogScale sll)
    ++ "\nreceiverWeightDiffs: " ++ show (Map.assocs $ receiverWeightDiffs sll)
    ++ "\nexpectedVarChanges" ++ show (Map.assocs $ expectedVarChanges sll)
    )

observedVarsSLL :: SenderLogLik -> Vector Double
observedVarsSLL sll = let
    rws = [ (r, fromIntegral w / n) | (r,w) <- Map.assocs rc ]
    in accumVector (+) (Vars.weightReceiverBy rws v h0 s) (Map.assocs ovc)
  where
    m = senderModel sll
    n = fromIntegral $ sendCount sll
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
    scale = log (meanInvWeight sll) - meanInvWeightLogScale sll
    rws = receiverWeightDiffs sll
    h0 = History.empty

valueGradSLL :: SenderLogLik -> (Double, Vector Double)
valueGradSLL sll =
    let x = observedVarsSLL sll
        mu = expectedVarsSLL sll
        f = valueSLL sll
        g = x `subVector` mu
    in if any isNaN (elemsVector g)
            then trace ("NaN Grad\n  obs: " ++ show (elemsVector x)
                        ++ "\n  exp: " ++ show (elemsVector mu)
                        ++ "\n  ecounts: " ++ show (expectedReceiverCountsSLL sll)
                        ) (f,g)
            else (f, g)

unionSLL :: SenderLogLik -> SenderLogLik -> SenderLogLik
unionSLL sll1 sll2 =
    if sendCount sll1 >= sendCount sll2
        then add sll1 sll2
        else add sll2 sll1
  where
    deleteLookup = Map.updateLookupWithKey (\_ _ -> Nothing)
    
    add (SenderLogLik m s sc1 rc1 ovc1 v1 miw1 logscale1 rw1 evc1)
        (SenderLogLik _ _ sc2 rc2 ovc2 v2 miw2 logscale2 rw2 evc2) = let
        
        scale = fromIntegral sc2 / (fromIntegral sc1 + fromIntegral sc2)
        scale' = 1 - scale
        update old new = old + scale * (new - old)
        

        updateMap old new = let
            with k v = v `seq` (k,v)
            (diff,kvs1) =
                mapAccumL (\acc (k,v) ->
                              if Map.null acc
                                  then (acc,with k $ scale' * v)
                                  else case deleteLookup k acc of
                                      (Nothing, acc') ->
                                          (acc', with k $ scale' * v)
                                      (Just v', acc') ->
                                          (acc', with k $ update v v'))
                          new
                          (Map.assocs old)
            kvs2 =[ with k (scale * v) | (k,v) <- Map.assocs diff ]
            
            in Map.fromList $ kvs1 ++ kvs2
        
        (miw, logscale) =
            if logscale1 <= logscale2
                  then ( update miw1 (miw2 * exp (logscale1 - logscale2))
                       , logscale1 )
                  else ( update (miw1 * exp (logscale2 - logscale1)) miw2
                       , logscale2 )

        in SenderLogLik m s
                        ((+) sc1 sc2)
                        (unionWith' (+) rc1 rc2)
                        (updateMap ovc1 ovc2)
                        (update v1 v2)
                        miw logscale
                        (updateMap rw1 rw2)
                        (updateMap evc1 evc2)



singletonSLL :: Model -> SenderId -> ([ReceiverId], History) -> SenderLogLik
singletonSLL m s (rs, h) = let
    sc  = length rs
    sc' = fromIntegral sc
    rc = Map.fromList $ zip rs (repeat 1)
    ovc = foldl' (flip $ \(i,dx) -> Map.insertWith' (+) i (dx / sc'))
                 Map.empty
                 (concatMap (Vars.dyadChanges vars h s) rs)

    -- active receiver set and difference in log-weights
    (active, dlws) = unzip [ (r,dlw)
                           | (r,dlw) <- Vars.mulSenderChangesBy beta vars h s
                           , Model.validDyad m s r
                           ]
                           
    -- rescale differences in log-weights
    logscale = foldl' max 0 dlws
    invscale = exp (-logscale)
    dlws' = [ dlw - logscale | dlw <- dlws ]

    -- initial log-probabilities and probabilities
    lp0s = map (Model.logProb m h0 s) active
    p0s = map exp lp0s
    
    -- log (scaled weights)
    lws' = [ lp0 + dlw' | (lp0, dlw') <- zip lp0s dlws' ]
    
    -- log (difference of weights)
    ldws'_p = [ lp0 + log (exp dlw' - invscale)
              | (lp0, dlw') <- zip lp0s dlws'
              , dlw' >= -logscale ]
    ldws'_n = [ lp0 + log (invscale - exp dlw')
              | (lp0, dlw') <- zip lp0s dlws'
              , dlw' < -logscale ]
           
    -- sum of scaled weights
    max_ldw'_p = foldl' max (-logscale) ldws'_p
    log_sum_w'_p = log (exp(-logscale - max_ldw'_p)
                       + foldl' (+) 0 [ exp (ldw'_p - max_ldw'_p) | ldw'_p <- ldws'_p ])
                   + max_ldw'_p
    max_ldw'_n = foldl' max (-1/0) ldws'_n
    log_sum_w'_n = max_ldw'_n
                   + if (not . isInfinite) max_ldw'_n
                         then log (foldl' (+) 0 [ exp (ldw'_n - max_ldw'_n)
                                                | ldw'_n <- ldws'_n ])
                         else 0
    log_sum_w' = log_sum_w'_p + log1p (-exp (log_sum_w'_n - log_sum_w'_p))
       

    log_sum_w = log_sum_w' + logscale
    lps = [ min 0 (lw' - log_sum_w') | lw' <- lws' ]
    
    v = mean [ findWithDefault (Model.logProb m h0 s r - log_sum_w)
                               r
                               (zip active lps)
             | r <- rs
             ]

    ps = map exp lps
    miw = 1
    miwscale = logscale + log_sum_w'
    dps = [ p - exp (lp0 - log_sum_w) | (p,lp0) <- zip ps lp0s ]
    rw = Map.fromList $ zip active dps
    evc = Map.fromList $
               Vars.weightReceiverChangesBy (zip active ps) vars h s
    in (if (not $ and [ p >= 0 && p <= 1 | p <- ps ]) 
                || isInfinite miw
                || isInfinite miwscale
            then trace (  ""
                          ++ "\ndlws: " ++ show dlws
                          ++ "\nlogscale: " ++ show logscale
                          ++ "\ninvscale: " ++ show invscale
                          ++ "\ndlws': " ++ show dlws'
                          ++ "\nlp0s: " ++ show lp0s
                          ++ "\np0s: " ++ show p0s
                          ++ "\nlws': " ++ show lws'
                          ++ "\nldws'_p: " ++ show ldws'_p
                          ++ "\nldws'_n: " ++ show ldws'_n
                          ++ "\nlog_sum_w'_p: " ++ show log_sum_w'_p
                          ++ "\nlog_sum_w'_n: " ++ show log_sum_w'_n
                          ++ "\nlog_sum_w': " ++ show log_sum_w'                       
                          ++ "\nlog_sum_w: " ++ show log_sum_w
                          ++ "\nlps: " ++ show lps
                          ++ "\nv: " ++ show v
                          ++ "\nps: " ++ show ps
                          ++ "\nmiw: " ++ show miw
                          ++ "\nmiwscale: " ++ show miwscale
                          ++ "\ndps: " ++ show dps
                          )
               else id)
        SenderLogLik m s sc rc ovc v miw miwscale rw evc
  where
    vars = Model.vars m
    beta = Model.coefs m
    h0 = History.empty
    findWithDefault a0 k kas = case lookup k kas of Just a  -> a
                                                    Nothing -> a0
foreign import ccall unsafe "expm1"
    expm1 :: Double -> Double

foreign import ccall unsafe "log1p"
    log1p :: Double -> Double


emptySLL :: Model -> SenderId -> SenderLogLik
emptySLL m s = SenderLogLik m s 0 Map.empty Map.empty 0 1 inf Map.empty Map.empty
  where
    inf = 1/0

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

mean :: (Fractional a) => [a] -> a
mean = go 0 0
  where
    go n xbar [] = xbar
    go n xbar (x:xs) = let
        diff = x - xbar
        n' = n + 1
        xbar' = diff/n' + xbar
        in n' `seq` xbar' `seq` go n' xbar' xs

weightedMean :: (Fractional a) => [(a,a)] -> a
weightedMean = go 0 0
  where
    go wtot xbar []          = xbar
    go wtot xbar ((w,x):wxs) = 
        if w == 0 then go wtot xbar wxs
                  else let
                      diff = x - xbar
                      wtot' = wtot + w
                      xbar' = xbar + (w/wtot')*diff
                      in wtot' `seq` xbar' `seq` go wtot' xbar' wxs

valueGrad :: LogLik -> (Double, Vector Double)
valueGrad (LogLik m _ sllm) = let
    (ws,fs,gs) = unzip3 [ let w = fromIntegral $ sendCount sll
                              (f,g) = valueGradSLL sll
                          in (w,f,g)
                        | sll <- Map.elems sllm
                        ]
    f = weightedMean $ zip ws fs
    g = weightedMeanVector (Vars.dim $ Model.vars m) $ zip ws gs
    in (f, g)

deviance :: LogLik -> Double
deviance = fst . devianceScore

score :: LogLik -> Vector Double
score = snd . devianceScore

devianceScore :: LogLik -> (Double, Vector Double)
devianceScore ll = let
    (f, g) = valueGrad ll
    scount = fromIntegral $ Map.size $ senderLogLik ll
    n = fromIntegral $ count ll
    in (-2 * n * f, scaleVector (sqrt n) g)

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
    foldl' (flip insert) (empty m) mhs

module LogLik (
    LogLik,
    empty,
    insert,
    
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
                 , sumLogWeight :: !Double
                 , sumInvWeight :: !Double
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
    [ (r, Map.findWithDefault (siw * Model.prob m h0 s r) r rws)
    | r <- Model.validReceivers m s
    ]
  where
    m = senderModel sll
    s = sender sll
    siw = sumInvWeight sll
    rws = receiverWeights sll
    h0 = History.empty

valueGradSLL :: SenderLogLik -> (Double, Vector Double)
valueGradSLL sll =
    let x = observedVarsSLL sll
        mu = expectedVarsSLL sll
        n = fromIntegral $ sendCount sll
        scale = Model.logSumWeights m h0 s
        f = x `dotVector` beta - (sumLogWeight sll + n * scale)
        g = x `subVector` mu
    in f `seq` (f, g)
  where
    m = senderModel sll
    s = sender sll
    beta = Model.coefs m
    h0 = History.empty

unionSLL :: SenderLogLik -> SenderLogLik -> SenderLogLik
unionSLL (SenderLogLik m s sc1 rc1 ovc1 slw1 siw1 rwc1 evc1)
         (SenderLogLik _ _ sc2 rc2 ovc2 slw2 siw2 rwc2 evc2) =
    SenderLogLik m s
                 ((+) sc1 sc2)
                 (unionWith' (+) rc1 rc2)
                 (unionWith' (+) ovc1 ovc2)
                 ((+) slw1 slw2)
                 ((+) siw1 siw2)
                 (unionWith' (+) rwc1 rwc2)
                 (unionWith' (+) evc1 evc2)

singletonSLL :: Model -> SenderId -> ([ReceiverId], History) -> SenderLogLik
singletonSLL m s (rs, h) = let
    sc = length rs
    rc = Map.fromList $ zip rs (repeat 1)
    ovc = foldl' (flip $ uncurry $ Map.insertWith' (+)) Map.empty $
              concatMap (Vars.dyadChanges v h s) rs
    rwds = [ let p0 = Model.prob m h0 s r 
                 w = p0 * exp d
             in (r, w, w - p0)
           | (r,d) <- Vars.mulSenderChangesBy beta v h s
           , Model.validDyad m s r
           ]
    sum_d = foldl' (+) 0 [ d | (_,_,d) <- rwds ]
    sum_w = 1 + sum_d
    slw = ((fromIntegral sc *) . log) sum_w
    siw = fromIntegral sc / sum_w
    rws = [ (r, w * siw) | (r,w,_) <- rwds ]
    rwc = Map.fromList rws
    evc = Map.fromList $ Vars.weightReceiverChangesBy rws v h s
    in SenderLogLik m s sc rc ovc slw siw rwc evc
  where
    v = Model.vars m
    beta = Model.coefs m
    h0 = History.empty

emptySLL :: Model -> SenderId -> SenderLogLik
emptySLL m s = SenderLogLik m s 0 Map.empty Map.empty 0 0 Map.empty Map.empty

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
    f = foldl' (+) 0 fs
    g = sumVector (Vars.dim $ Model.vars m) gs
    in g `seq` f `seq` (f, g)

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

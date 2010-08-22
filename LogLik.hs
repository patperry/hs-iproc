module LogLik (
    LogLik,
    empty,
    insert,
    
    deviance,
    nullDeviance,   
    nullDf,
    residDf,
    
    ) where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import Actor( ReceiverId, SenderId )
import History( History )
import Message( Message(..) )
import Model( Model )
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
                 }
    deriving (Show)

valueSLL :: SenderLogLik -> Double
valueSLL (SenderLogLik m s sc rc ovc slw) =
    let x0 = Vars.weightReceiverBy
                 [(r, fromIntegral c) | (r,c) <- Map.assocs rc ]
                 v h0 s
        x = accumVector (+) x0 $ Map.assocs ovc
        scale = Model.logSumWeights m h0 s
        n = fromIntegral sc
    in x `dotVector` beta - (slw + n * scale)
  where
    v = Model.vars m
    beta = Model.coefs m
    h0 = History.empty

unionSLL :: SenderLogLik -> SenderLogLik -> SenderLogLik
unionSLL (SenderLogLik m s sc1 rc1 ovc1 slw1)
         (SenderLogLik _ _ sc2 rc2 ovc2 slw2) =
    SenderLogLik m s
                 ((+) sc1 sc2)
                 (unionWith' (+) rc1 rc2)
                 (unionWith' (+) ovc1 ovc2)
                 ((+) slw1 slw2)

singletonSLL :: Model -> SenderId -> ([ReceiverId], History) -> SenderLogLik
singletonSLL m s (rs, h) = let
    sc = length rs
    rc = Map.fromList $ zip rs (repeat 1)
    ovc = foldl' (flip $ uncurry $ Map.insertWith' (+)) Map.empty $
              concatMap (Vars.dyadChanges v h s) rs
    slw = ((fromIntegral sc *) . log1p) $ foldl' (+) 0
              [ Model.prob m h0 s r * expm1 d
              | (r,d) <- Vars.mulSenderChangesBy beta v h s
              , Model.validDyad m s r
              ]
    in SenderLogLik m s sc rc ovc slw
  where
    v = Model.vars m
    beta = Model.coefs m
    h0 = History.empty
    log1p = log . (1+)
    expm1 = ((-1)+) . exp

emptySLL :: Model -> SenderId -> SenderLogLik
emptySLL m s = SenderLogLik m s 0 Map.empty Map.empty 0

insertSLL :: ([ReceiverId], History) -> SenderLogLik -> SenderLogLik
insertSLL msg sll =
    unionSLL sll $ singletonSLL (senderModel sll) (sender sll) msg

data LogLik = LogLik { model :: !Model
                     , senderLogLik :: !(Map SenderId SenderLogLik)
                     }
    deriving (Show)

value :: LogLik -> Double
value (LogLik _ sllm) =
    foldl' (+) 0 [ valueSLL sll | sll <- Map.elems sllm ]

deviance :: LogLik -> Double
deviance ll = -2 * value ll

nullDeviance :: LogLik -> Double
nullDeviance (LogLik m sllm) =
    2 * foldl' (+) 0
        [ ( fromIntegral (sendCount sll)
          * log (fromIntegral $ length $ Model.validReceivers m s)
          )
        | (s,sll) <- Map.assocs sllm
        ]

nullDf :: LogLik -> Int
nullDf (LogLik _ sllm) =
    foldl' (+) 0 [ sendCount sll | sll <- Map.elems sllm ]

residDf :: LogLik -> Int
residDf ll@(LogLik m _) = 
    nullDf ll - Vars.dim (Model.vars m)

empty :: Model -> LogLik
empty m = LogLik m Map.empty

union :: LogLik -> LogLik -> LogLik
union (LogLik m sll1) (LogLik _ sll2) =
    LogLik m $ unionWith' unionSLL sll1 sll2

singleton :: Model -> (Message, History) -> LogLik
singleton m (Message s rs, h) =
    LogLik m $ Map.singleton 1 $ singletonSLL m s (rs,h)

insert :: (Message, History) -> LogLik -> LogLik
insert msg ll =
    union ll $ singleton (model ll) msg

module Model (
    Model,
    Loops(..),
    fromVars,

    vars,
    coefs,
    hasLoops,
    senders,
    receivers,
    validDyad,
    validReceivers,
    
    weight,
    logWeight,
    logWeightChange,

    weights,
    logWeights,
    logWeightChanges,
    sumWeights,

    prob,
    probs,
    meanVars,
    covVars,
    ) where
        
import Data.List( foldl' )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import Actor( SenderId, ReceiverId )
import History( History )
import Vars( Vars )
import qualified Vars as Vars

data Loops = Loops | NoLoops deriving (Eq, Show)

data Model =
    Model { vars :: !Vars
          , coefs :: !(Vector Double)
          , hasLoops :: !Bool
          }
    deriving (Show)

fromVars :: Vars -> Vector Double -> Loops -> Model
fromVars v c l
    | Vars.dim v /= dimVector c =
        error "fromVars: dimension mismatch"
    | otherwise =
        Model v c $ case l of { Loops -> True ; NoLoops -> False }

validDyad :: Model -> SenderId -> ReceiverId -> Bool
validDyad m s r | hasLoops m = True
                | otherwise  = s /= r

senders :: Model -> [SenderId]
senders m = Vars.senders $ vars m

receivers :: Model -> [ReceiverId]
receivers m = Vars.receivers $ vars m

validReceivers ::  Model ->  SenderId ->[ReceiverId]
validReceivers m s = filter (validDyad m s) $ receivers m

weight :: Model -> History -> SenderId -> ReceiverId -> Double
weight m h s r = exp $ logWeight m h s r

logWeight :: Model -> History -> SenderId -> ReceiverId -> Double
logWeight m h s r | not (validDyad m s r) = neginf
                  | otherwise =
    Vars.mulDyadBy (coefs m) (vars m) h s r
  where
    neginf = -1.0/(0.0 :: Double)

logWeightChange :: Model -> History -> SenderId -> ReceiverId -> Double
logWeightChange m h s r | not (validDyad m s r) = 0
                        | otherwise =
    Vars.mulDyadChangesBy (coefs m) (vars m) h s r

sumWeights :: Model -> History -> SenderId -> Double
sumWeights m h s =
    foldl' (+) 0 $ (snd . unzip) (weights m h s)

weights :: Model -> History -> SenderId -> [(ReceiverId, Double)]
weights m h s = [ (r, exp lw) | (r,lw) <- logWeights m h s ]

logWeights :: Model -> History -> SenderId -> [(ReceiverId, Double)]
logWeights m h s = 
    filter (validDyad m s . fst) $
        Vars.mulSenderBy (coefs m) (vars m) h s

logWeightChanges :: Model -> History -> SenderId -> [(ReceiverId, Double)]
logWeightChanges m h s = 
    filter (validDyad m s . fst) $ 
        Vars.mulSenderChangesBy (coefs m) (vars m) h s

prob :: Model -> History -> SenderId -> ReceiverId -> Double
prob m h s r = let
    lws = snd $ unzip $ logWeights m h s
    lw_max = foldl' max neginf lws
    w_sum = foldl' (+) 0 [ exp (lw - lw_max) | lw <- lws ]
    in if w_sum == 0
           then 0
           else min 1 $ exp (logWeight m h s r - lw_max) / w_sum
  where
    neginf = -1/0 :: Double 

probs :: Model -> History -> SenderId -> [(ReceiverId, Double)]
probs m h s = let
    rlws = logWeights m h s
    lws = snd $ unzip rlws
    lw_max = foldl' max neginf lws
    rws = [ (r, exp (lw - lw_max)) | (r, lw) <- rlws ]
    w_sum = foldl' (+) 0 (snd $ unzip rws)
    in if w_sum == 0 then rws
                     else [ (r, w / w_sum) | (r,w) <- rws ]
  where
    neginf = -1/0 :: Double

meanVars :: Model -> History -> SenderId -> Vector Double
meanVars m h s = fst $ meanCovVars m h s

covVars :: Model -> History -> SenderId -> Herm Matrix Double
covVars m h s = snd $ meanCovVars m h s

meanCovVars :: Model -> History -> SenderId -> (Vector Double, Herm Matrix Double)
meanCovVars m h s = let
    v = vars m
    wxs = [ (w, Vars.dyad v h s r) | (r,w) <- probs m h s ]
    mu = weightedMeanVector (Vars.dim v) wxs
    sigma = weightedCovMatrixWithMean mu MLCov wxs
    in (mu, sigma)

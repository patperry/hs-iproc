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
    
    dyadWeight,
    dyadLogWeight,
    dyadLogWeightChange,

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

validReceivers :: SenderId -> Model -> [ReceiverId]
validReceivers s m = filter (validDyad m s) $ receivers m

dyadWeight :: Model -> History -> SenderId -> ReceiverId -> Double
dyadWeight m h s r = exp $ dyadLogWeight m h s r

dyadLogWeight :: Model -> History -> SenderId -> ReceiverId -> Double
dyadLogWeight m h s r | not (validDyad m s r) = neginf
                      | otherwise =
    Vars.mulDyadBy (coefs m) (vars m) h s r
  where
    neginf = -1.0/(0.0 :: Double)

dyadLogWeightChange :: Model -> History -> SenderId -> ReceiverId -> Double
dyadLogWeightChange m h s r | not (validDyad m s r) = 0
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
prob m h s r = dyadWeight m h s r / sumWeights m h s

probs :: Model -> History -> SenderId -> [(ReceiverId, Double)]
probs m h s = let
    rws = weights m h s
    w_sum = foldl' (+) 0 (snd $ unzip rws)
    in [ (r, w / w_sum) | (r,w) <- rws ]

meanVars :: Model -> History -> SenderId -> Vector Double
meanVars m h s = fst $ meanCovVars m h s

covVars :: Model -> History -> SenderId -> Herm Matrix Double
covVars m h s = snd $ meanCovVars m h s

meanCovVars :: Model -> History -> SenderId -> (Vector Double, Herm Matrix Double)
meanCovVars m h s = let
    v = vars m
    wxs = [ (w, Vars.dyad v h s r) | (r,w) <- weights m h s ]
    mu = weightedMeanVector (Vars.dim v) wxs
    sigma = weightedCovMatrixWithMean mu MLCov wxs
    in (mu, sigma)

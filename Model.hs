module Model (
    Model,
    Loops(..),
    fromVars,
    addStep,

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
    logSumWeights,

    prob,
    logProb,
    probs,
    logProbs,
    meanVars,
    covVars,
    meanCovVars,
    ) where
        
import ModelBase( Loops(..) )
import qualified ModelBase as Base

import Data.Map( Map )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import History( History )
import Types( SenderId, ReceiverId )
import Vars( Vars )
import qualified History as History


data Model =
    Model { base :: !(Base.Model)
          , staticProbs :: !(Map (SenderId, ReceiverId) Double)
          }
    deriving (Show)

fromBase :: Base.Model -> Model
fromBase m = let
    h0 = History.empty
    sps = Map.fromList [ ((s,r),p) | s <- Base.senders m
                                   , (r,p) <- Base.probs m h0 s ]
    in Model m sps

fromVars :: Vars -> Vector Double -> Loops -> Model
fromVars v c l =
    fromBase $ Base.fromVars v c l

addStep :: Vector Double -> Model -> Model
addStep step m0 = let
    m = Base.addStep step (base m0)
    in fromBase m

vars :: Model -> Vars
vars = Base.vars . base

coefs :: Model -> Vector Double
coefs = Base.coefs . base

hasLoops :: Model -> Bool
hasLoops = Base.hasLoops . base

validDyad :: Model -> SenderId -> ReceiverId -> Bool
validDyad = Base.validDyad . base

senders :: Model -> [SenderId]
senders = Base.senders . base

receivers :: Model -> [ReceiverId]
receivers = Base.receivers . base

validReceivers ::  Model ->  SenderId -> [ReceiverId]
validReceivers = Base.validReceivers . base

weight :: Model -> History -> SenderId -> ReceiverId -> Double
weight = Base.weight . base

logWeight :: Model -> History -> SenderId -> ReceiverId -> Double
logWeight = Base.logWeight . base

logWeightChange :: Model -> History -> SenderId -> ReceiverId -> Double
logWeightChange = Base.logWeightChange . base

sumWeights :: Model -> History -> SenderId -> Double
sumWeights = Base.sumWeights . base

logSumWeights :: Model -> History -> SenderId -> Double
logSumWeights = Base.logSumWeights . base

weights :: Model -> History -> SenderId -> [(ReceiverId, Double)]
weights = Base.weights . base

logWeights :: Model -> History -> SenderId -> [(ReceiverId, Double)]
logWeights = Base.logWeights . base

logWeightChanges :: Model -> History -> SenderId -> [(ReceiverId, Double)]
logWeightChanges = Base.logWeightChanges . base

prob :: Model -> History -> SenderId -> ReceiverId -> Double
prob m h s r
    | History.null h = Map.findWithDefault 0 (s,r) $ staticProbs m
    | otherwise      = Base.prob (base m) h s r

logProb :: Model -> History -> SenderId -> ReceiverId -> Double
logProb = Base.logProb . base

probs :: Model -> History -> SenderId -> [(ReceiverId, Double)]
probs = Base.probs . base

logProbs :: Model -> History -> SenderId -> [(ReceiverId, Double)]
logProbs = Base.logProbs . base

meanVars :: Model -> History -> SenderId -> Vector Double
meanVars = Base.meanVars . base

covVars :: Model -> History -> SenderId -> Herm Matrix Double
covVars = Base.covVars . base

meanCovVars :: Model -> History -> SenderId -> (Vector Double, Herm Matrix Double)
meanCovVars = Base.meanCovVars . base

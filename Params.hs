module Params (
    Params(..),
    defaultParams,
    senders,
    receivers,
    validDyad,
    
    weight,
    staticWeight,
    
    logWeight,
    staticLogWeight,
    dynamicLogWeight,
    
    sumWeights,
    staticSumWeights,
    
    prob,
    staticProb,
    probs,
    
    expectedSVars,
    expectedDVars,
    ) where
        
import Data.List( foldl' )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import Actor( SenderId, ReceiverId )
import qualified Intervals as Intervals
import SVars( SVars )
import qualified SVars as SVars
import DVars( DVars, History, DVar(..) )
import qualified DVars as DVars


data Params =
    Params { svars :: !SVars
           , dvars :: !DVars
           , scoefs :: !(Vector Double)
           , dcoefs :: !(Vector Double)
           , hasSelfLoops :: !Bool
           }
    deriving (Eq, Show)

           
defaultParams :: SVars -> DVars -> Params
defaultParams sv dv =
    Params sv
           dv
           (constantVector (SVars.dim sv) 0)
           (constantVector (DVars.dim dv) 0)
           False

validDyad :: SenderId -> ReceiverId ->  Params -> Bool
validDyad s r p | hasSelfLoops p = True
                | otherwise      = s /= r

senders :: Params -> [SenderId]
senders p = Map.keys $ SVars.senders $ svars p

receivers :: SenderId -> Params -> [ReceiverId]
receivers s p = 
    filter (\r -> validDyad s r p) $
        Map.keys $ SVars.receivers $ svars p


logWeight :: History -> SenderId -> ReceiverId -> Params -> Double
logWeight c s r p =
    staticLogWeight s r p + dynamicLogWeight c s r p

staticLogWeight :: SenderId -> ReceiverId -> Params -> Double
staticLogWeight s r p@(Params sv dv sc _ _) | not $ validDyad s r p = -1/0
                                            | otherwise = let
    x0 = SVars.lookupDyad s r sv
    lw0 = dotVector x0 sc
    in lw0
    
dynamicLogWeight :: History -> SenderId -> ReceiverId -> Params -> Double
dynamicLogWeight c s r p@(Params sv dv _ dc _) | not $ validDyad s r p = -1/0
                                               | otherwise = let
    ds = DVars.lookupDyad c s r dv
    lw1 = foldl' (+) 0 [ atVector dc (DVars.index d dv) | d <- ds ]
    in lw1

weight :: History -> SenderId -> ReceiverId -> Params -> Double
weight c s r p = exp (logWeight c s r p)

staticWeight :: SenderId -> ReceiverId -> Params -> Double
staticWeight s r p = exp (staticLogWeight s r p)

sumWeights :: History -> SenderId -> Params -> Double
sumWeights c s p = let
    ws = map (\r -> weight c s r p) $ receivers s p
    in foldl' (+) 0 ws

staticSumWeights :: SenderId -> Params -> Double
staticSumWeights s p = let
    ws = map (\r -> staticWeight s r p) $ receivers s p
    in foldl' (+) 0 ws
    
prob :: History -> SenderId -> ReceiverId -> Params -> Double
prob c s r p = weight c s r p / sumWeights c s p

staticProb :: SenderId -> ReceiverId -> Params -> Double
staticProb s r p = staticWeight s r p / staticSumWeights s p

probs :: History -> SenderId -> Params -> [(ReceiverId, Double)]
probs c s p = [ (r, prob c s r p) | r <- receivers s p ]

expectedSVars :: History -> SenderId -> Params -> Vector Double
expectedSVars c s p = SVars.sumWithSender s (probs c s p) (svars p)

expectedDVars :: History -> SenderId -> Params -> [(DVar, Double)]
expectedDVars c s p = Map.assocs $
    foldl' (\m (r,w) ->
        foldl' (flip $ \v -> Map.insertWith' (+) v w)
               m
               (DVars.lookupDyad c s r dv))
        Map.empty
        (probs c s p)
  where
    dv = dvars p

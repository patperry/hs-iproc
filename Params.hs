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
    ) where
        
import Data.List( foldl' )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import Actor( SenderId, ReceiverId )
import qualified Intervals as Intervals
import SVars( SVars )
import qualified SVars as SVars
import DVars( DVars, Context, DVar(..) )
import qualified DVars as DVars


data Params =
    Params { svars :: !SVars
           , dvars :: !DVars
           , staticCoefs :: !(Vector Double)
           , sendCoefs :: !(Vector Double)
           , receiveCoefs :: !(Vector Double)
           , hasSelfLoops :: !Bool
           }
    deriving (Eq, Show)

           
defaultParams :: SVars -> DVars -> Params
defaultParams sv dv =
    Params sv
           dv
           (constantVector (SVars.dim sv) 0)
           (constantVector (Intervals.size $ DVars.sendIntervals dv) 0)
           (constantVector (Intervals.size $ DVars.receiveIntervals dv) 0)
           False

logWeight :: Context -> SenderId -> ReceiverId -> Params -> Double
logWeight c s r p =
    staticLogWeight s r p + dynamicLogWeight c s r p

staticLogWeight :: SenderId -> ReceiverId -> Params -> Double
staticLogWeight s r p@(Params sv dv c0 _ _ _) | not $ validDyad s r p = -1/0
                                              | otherwise = let
    x0 = SVars.lookupDyad s r sv
    lw0 = dotVector x0 c0
    in lw0
    
dynamicLogWeight :: Context -> SenderId -> ReceiverId -> Params -> Double
dynamicLogWeight c s r p@(Params sv dv _ cs cr _) | not $ validDyad s r p = -1/0
                                                   | otherwise = let
    ds = DVars.lookupDyad c s r dv
    lw1 = foldl' (+) 0 [ case d of Send i -> atVector cs i
                                   Receive i' -> atVector cr i' 
                       | d <- ds
                       ]
    in lw1

weight :: Context -> SenderId -> ReceiverId -> Params -> Double
weight c s r p = exp (logWeight c s r p)

staticWeight :: SenderId -> ReceiverId -> Params -> Double
staticWeight s r p = exp (staticLogWeight s r p)

sumWeights :: Context -> SenderId -> Params -> Double
sumWeights c s p = let
    ws = map (\r -> weight c s r p) $ receivers s p
    in foldl' (+) 0 ws

staticSumWeights :: SenderId -> Params -> Double
staticSumWeights s p = let
    ws = map (\r -> staticWeight s r p) $ receivers s p
    in foldl' (+) 0 ws
    
prob :: Context -> SenderId -> ReceiverId -> Params -> Double
prob c s r p = weight c s r p / sumWeights c s p

staticProb :: SenderId -> ReceiverId -> Params -> Double
staticProb s r p = staticWeight s r p / staticSumWeights s p

validDyad :: SenderId -> ReceiverId ->  Params -> Bool
validDyad s r p | hasSelfLoops p = True
                | otherwise      = s /= r

senders :: Params -> [SenderId]
senders p = Map.keys $ SVars.senders $ svars p

receivers :: SenderId -> Params -> [ReceiverId]
receivers s p = 
    filter (\r -> validDyad s r p) $
        Map.keys $ SVars.receivers $ svars p

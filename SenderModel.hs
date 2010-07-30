module SenderModel (
    SenderModel,
    params,
    sender,
    dvars,
    
    senderModel,
    update,

    prob,
    logProb,
    dvarProbs,
    
    weight,
    staticWeight,
    weightDiff,

    weightSum,
    weightDiffSum,
    staticWeights,
    weightDiffs,
    ) where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor( Receiver, ReceiverId, Sender, SenderId )
import DVars( DVars, DVar(..) )
import qualified DVars as DVars
import Params( Params )
import qualified Params as Params
import SVars( SVars )
import qualified SVars as SVars


data SenderModel = 
    SenderModel { params :: !Params
                , sender :: !SenderId
                , dvars :: !DVars
                , static :: !StaticWeights
                , dynamic :: !DynamicWeights
                }
    deriving (Eq, Show)

data StaticWeights =
    StaticWeights { staticWeightLogPairMap :: !(Map ReceiverId (Double, Double))
                  , staticWeightSum :: !Double
                  }
    deriving (Eq, Show)

data DynamicWeights =
    DynamicWeights { dynamicWeightDiffPairMap :: !(Map ReceiverId (DVar, Double, Double))
                   , dynamicWeightSum :: !Double
                   , dynamicWeightDiffSum :: !Double
                   }
    deriving (Eq, Show)


senderModel :: Params -> SenderId -> DVars -> SenderModel
senderModel p s dv = let
    sw = makeStaticWeights p s
    dw = makeDynamicWeights p s sw dv
    in SenderModel p s dv sw dw

update :: DVars -> SenderModel -> SenderModel
update dv (SenderModel p s _ sw _) =
    SenderModel p s dv sw $ makeDynamicWeights p s sw dv

weightSum :: SenderModel -> Double
weightSum = dynamicWeightSum . dynamic

weightDiffSum :: SenderModel -> Double
weightDiffSum = dynamicWeightDiffSum . dynamic

prob :: ReceiverId -> SenderModel -> Double
prob r sm = weight r sm / weightSum sm

logProb :: ReceiverId -> SenderModel -> Double
logProb r sm = log (prob r sm)

weight :: ReceiverId -> SenderModel -> Double
weight r sm =
    case Map.lookup r (dynamicWeightDiffPairMap $ dynamic sm) of
        Just (_,w,_) -> w
        Nothing -> staticWeight r sm

staticWeight :: ReceiverId -> SenderModel -> Double
staticWeight r sm  =
    fst $ staticWeightLogPair r $ static sm

weightDiff :: ReceiverId -> SenderModel -> Double
weightDiff r sm =
    case Map.lookup r (dynamicWeightDiffPairMap $ dynamic sm) of
        Just (_,_,d) -> d
        Nothing -> 0
    
staticWeights :: SenderModel -> [(ReceiverId, Double)]
staticWeights sm =
    [ (r,w)
    | (r, (w,_)) <- Map.toList $ staticWeightLogPairMap $ static sm
    ]

weightDiffs :: SenderModel -> [(ReceiverId, Double)]
weightDiffs sm =
    [ (r,d)
    | (r, (v,_,d)) <- Map.toList $ dynamicWeightDiffPairMap $ dynamic sm
    ]

dvarProbs :: SenderModel -> [(DVar, Double)]
dvarProbs sm =
    [ (v,w / w_sum )
    | (v,w,_) <- Map.elems $ dynamicWeightDiffPairMap $ dynamic sm
    ]
  where
    w_sum = weightSum sm

makeStaticWeights :: Params -> SenderId -> StaticWeights
makeStaticWeights p s = let
    wm = Map.fromList [ let w = weight x in w `seq` (r,w)
                      | (r, x) <- SVars.lookupSender s sv
                      , validReceiver r
                      ]
                      
    w_sum = foldl' (+) 0 $ map fst $ Map.elems wm
    
    in StaticWeights wm w_sum
  where
    sv = Params.svars p
    coefs = Params.staticCoefs p
    validReceiver r = Params.validDyad (s,r) p
    
    weight x = let
        lw = coefs `dotVector` x
        w = exp lw
        in lw `seq` w `seq` (w,lw)

staticWeightLogPair :: ReceiverId -> StaticWeights -> (Double, Double)
staticWeightLogPair r sw = 
    Map.findWithDefault (0, neginf)
                        r (staticWeightLogPairMap sw)
  where
    neginf = -1/0

makeDynamicWeights :: Params -> SenderId -> StaticWeights -> DVars -> DynamicWeights
makeDynamicWeights p s sw dv = let
    rws = flip map (DVars.lookupSender s dv) $ \(r,v) -> let
        (w0, eta0) = staticWeightLogPair r sw
        eta_diff = logWeight v
        eta = eta0 + eta_diff
        w = exp eta
        diff = w - w0
        in w `seq` diff `seq` (r,(v,w,diff))
        
    diff_sum = foldl' (+) 0 $ [ diff | (_,(_,_,diff)) <- rws ]
    w_sum = staticWeightSum sw + diff_sum
    
    in DynamicWeights (Map.fromList rws) w_sum diff_sum
              
  where
    scoefs = Params.sendCoefs p
    rcoefs = Params.receiveCoefs p
    
    logWeight (Send i) = atVector scoefs i
    logWeight (Receive i') = atVector rcoefs i'
    logWeight (SendAndReceive i i') = atVector scoefs i + atVector rcoefs i'

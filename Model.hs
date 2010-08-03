module Model (
    SenderModel,
    senderModel,
    
    params,
    sender,

    ReceiverModel,
    receiverModel,
    staticReceiverModel,

    staticPart,
    receivers,
    
    prob,
    probs,
    probParts,
    
    expectedSVars,
    expectedDVars,
    expectedDVarsByReceiver,

    ) where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor( Receiver, ReceiverId, Sender, SenderId )
import DVars( DVars, DVar(..), Context )
import qualified DVars as DVars
import Params( Params )
import qualified Params as Params
import SVars( SVars )
import qualified SVars as SVars

data StaticWeight = 
    StaticWeight { staticVars :: !(Vector Double)
                 , staticWeight :: !Double
                 , staticLogWeight :: !Double
                 , staticProb :: !Double
                 }
    deriving (Eq, Show)

data StaticWeights =
    StaticWeights { staticWeightMap :: !(Map ReceiverId StaticWeight)
                  , staticWeightSum :: !Double
                  }
    deriving (Eq, Show)
                    
data DynamicWeight =
    DynamicWeight { dynamicVars :: ![DVar]
                  , dynamicProb :: !Double
                  , dynamicDiffProb :: !Double
                  }
    deriving (Eq, Show)

data DynamicWeights =
    DynamicWeights { dynamicWeightMap :: !(Map ReceiverId DynamicWeight)
                   , dynamicDiffWeightSum :: !Double
                   }
    deriving (Eq, Show)
    
data ReceiverModel = ReceiverModel !StaticWeights !DynamicWeights
    deriving (Eq, Show)

receivers :: ReceiverModel -> [ReceiverId]
receivers (ReceiverModel sw _) = Map.keys $ staticWeightMap sw

prob :: ReceiverModel -> ReceiverId -> Double
prob (ReceiverModel sw dw) r =
    case Map.lookup r (dynamicWeightMap dw) of
        Just (DynamicWeight _ p _) -> p
        Nothing -> case Map.lookup r (staticWeightMap sw) of
            Just (StaticWeight _ _ _ p) -> p
            Nothing -> 0

probs :: ReceiverModel -> [(ReceiverId, Double)]
probs rm = [ (r, prob rm r) | r <- receivers rm ]

probParts :: ReceiverModel -> (Double, [(ReceiverId, Double)])
probParts (ReceiverModel sw dw) =
    ( 1 / (1 + dynamicDiffWeightSum dw / staticWeightSum sw)
    , [(r,d) | (r, DynamicWeight _ _ d) <- Map.assocs (dynamicWeightMap dw) ]
    )

expectedSVars :: ReceiverModel -> Vector Double
expectedSVars (ReceiverModel sw dw) =
    foldl' (flip $ \(x,p) -> addVectorWithScale p x 1) zero xps
  where
    xps =
        [ (x, case Map.lookup r (dynamicWeightMap dw) of
                  Just (DynamicWeight _ p' _) -> p'
                  Nothing -> p
          )
        | (r, StaticWeight x _ _ p) <- Map.assocs (staticWeightMap sw)
        ]
    zero = constantVector (dimVector $ fst $ head xps) 0

expectedDVars :: ReceiverModel -> [(DVar, Double)]
expectedDVars (ReceiverModel _ dw) = Map.assocs $
    foldl' (flip $ \(v,p) -> Map.insertWith' (+) v p) Map.empty $
        concat [ [(v,p) | v <- vs]
               | (DynamicWeight vs p _) <- Map.elems (dynamicWeightMap dw)
               ]

expectedDVarsByReceiver :: ReceiverModel -> [(ReceiverId, ([DVar], Double))]
expectedDVarsByReceiver (ReceiverModel _ dw) =
    [ (r, (vs, p))
    | (r, DynamicWeight vs p _) <- Map.assocs (dynamicWeightMap dw)
    ]

staticPart :: ReceiverModel -> ReceiverModel
staticPart (ReceiverModel sw _) =
    (ReceiverModel sw (DynamicWeights Map.empty 0))

data SenderModel = 
    SenderModel { params :: !Params
                , sender :: !SenderId
                , staticWeights :: !StaticWeights
                }
    deriving (Eq, Show)

    
senderModel :: Params -> SenderId -> SenderModel
senderModel p s = let
    rws = [ (r, weight x)
          | (r, x) <- SVars.lookupSender s sv
          , validReceiver r
          ]
                      
    w_sum = foldl' (+) 0 $ [ w | (_, StaticWeight _ w _ _) <- rws ]
    
    wm = Map.fromList 
             [ let sw = StaticWeight x w lw (w/w_sum) in sw `seq` (r, sw)
             | (r, StaticWeight x w lw _) <- rws
             ]
    
    in SenderModel p s $ StaticWeights wm w_sum

  where
    sv = Params.svars p
    coefs = Params.staticCoefs p
    validReceiver r = Params.validDyad (s,r) p
    
    weight x = let
        lw = coefs `dotVector` x
        w = exp lw
        in StaticWeight x w lw 0

        
staticReceiverModel :: SenderModel -> ReceiverModel
staticReceiverModel (SenderModel _ _ sw) =
    ReceiverModel sw (DynamicWeights Map.empty 0)

receiverModel :: Context -> SenderModel -> ReceiverModel
receiverModel c (SenderModel p s sw) = let
    rws = flip map (DVars.lookupSender c s dv) $ \(r,vs) -> let
        (StaticWeight _ w0 lw0 p0) =
            Map.findWithDefault (error "unknown or invalid receiver") r
                                (staticWeightMap sw)
        lw = lw0 + logWeight vs
        w = exp lw
        dw = w - w0
        in (r, (vs,w,dw,p0))
    
    diff_sum = foldl' (+) 0 [ dw | (_,(_,_,dw,_)) <- rws ]
    w_sum = staticWeightSum sw + diff_sum
    
    wm = Map.fromList
             [ let (p, dp) = (w/w_sum, p-p0) 
                   dw = DynamicWeight vs p dp
               in dw `seq` (r,dw)
             | (r, (vs,w,_,p0)) <- rws
             ]
                
    in ReceiverModel sw $ DynamicWeights wm diff_sum

  where
    dv = Params.dvars p
    scoefs = Params.sendCoefs p
    rcoefs = Params.receiveCoefs p
    
    logWeight = foldl' (+) 0 . map logWeightVar
    logWeightVar (Send i) = atVector scoefs i
    logWeightVar (Receive i') = atVector rcoefs i'
module SenderModel (
    SenderModel,
    params,
    dvars,
    
    staticWeightSum,
    staticWeight,
    staticLogWeight,
    staticWeightLogPair,
    
    ) where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor( Receiver, ReceiverId, Sender, SenderId )
import DVars( DVars )
import qualified DVars as DVars
import Params( Params )
import qualified Params as Params
import SVars( SVars )
import qualified SVars as SVars


data SenderModel =
    SenderModel { params :: !Params
                , dvars :: !DVars
                , staticWeightLogMap :: !(Map ReceiverId (Double, Double))
                , staticWeightSum :: !Double
                } deriving (Eq, Show)

senderModel :: Params -> DVars -> SenderId -> SenderModel
senderModel p dv s = let
    wm = Map.fromList [ let w = weight x in w `seq` (r,w)
                      | (r, x) <- SVars.lookupSender s sv
                      , validReceiver r
                      ]
                      
    w_sum = foldl' (+) 0 $ map fst $ Map.elems wm
    
    in SenderModel p dv wm w_sum
    
  where
    sv = Params.svars p
    coefs = Params.staticCoefs p
    validReceiver r = Params.validDyad (s,r)p
    
    weight x = let
        lw = coefs `dotVector` x
        w = exp lw
        in lw `seq` w `seq` (w,lw)

staticWeight :: SenderModel -> ReceiverId -> Double
staticWeight m = fst . staticWeightLogPair m

staticLogWeight :: SenderModel -> ReceiverId -> Double
staticLogWeight m = snd . staticWeightLogPair m

staticWeightLogPair :: SenderModel -> ReceiverId -> (Double, Double)
staticWeightLogPair m r = 
    Map.findWithDefault (error "invalid or unknown receiver")
                        r (staticWeightLogMap m)

module Params (
    Params,
    senderSet,
    receiverSet,
    staticCoefs,
    sendCoefs,
    receiveCoefs,
    hasSelfLoops,
    
    defaultParams,
    withStaticCoefs,
    withSendCoefs,
    withReceiveCoefs,
    withSelfLoops,
    
    validDyad,
    
    staticWeight,
    staticLogWeight,
    staticWeightLogPair,
    staticWeightSum,
    
    ) where
        
import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor( ReceiverId, SenderId, actorId )
import ActorSet( ActorSet )
import qualified ActorSet as ActorSet
import qualified IntervalSet as IntervalSet
import SVars( SVars )
import qualified SVars as SVars
import DVars( DVars )
import qualified DVars as DVars

data SenderParams =
    SenderParams { senderStaticWeightLog :: !(Map ReceiverId (Double, Double))
                 , senderStaticWeightSum :: !Double
                 } deriving (Eq, Show)

data Params =
    Params { senderSet :: !ActorSet
           , receiverSet :: !ActorSet
           , svars :: !SVars
           , staticCoefs :: !(Vector Double)
           , sendCoefs :: !(Vector Double)
           , receiveCoefs :: !(Vector Double)
           , hasSelfLoops :: !Bool
           , senderParams :: (Map SenderId SenderParams)
           }
    deriving (Eq, Show)


senderParamsMap :: Params -> Map SenderId SenderParams
senderParamsMap p@(Params ss rs sv coefs _ _ _ _) =
    Map.fromList [ (s, senderParams s)
                 | s <- map actorId $ ActorSet.toList ss ]
  where
    weight x = let
        lw = coefs `dotVector` x
        w = exp lw
        in lw `seq` w `seq` (w,lw)
        
    senderParams s = let
        wm = Map.fromList [ let w = weight x in w `seq` (r,w)
                          | (r, x) <- SVars.lookupSender s sv
                          , validDyad (s,r) p
                          ]
        w_sum = foldl' (+) 0 $ map fst $ Map.elems wm
        in SenderParams wm w_sum
           
defaultParams :: ActorSet -> ActorSet -> SVars -> DVars -> Params
defaultParams ss rs sv dv = let
    p = Params ss
               rs
               sv
               (constantVector (SVars.dim sv) 0)
               (constantVector (IntervalSet.size $ DVars.sendIntervals dv) 0)
               (constantVector (IntervalSet.size $ DVars.receiveIntervals dv) 0)
               False
               (senderParamsMap p)
    in p

withStaticCoefs :: Vector Double -> Params -> Params
withStaticCoefs coefs' (Params ss rs sv coefs scoefs rcoefs loops _)
    | dimVector coefs' /= dimVector coefs =
        error "dimension mismatch"
    | otherwise = let
        p = Params ss rs sv coefs' scoefs rcoefs loops (senderParamsMap p)
        in p

withSendCoefs :: Vector Double -> Params -> Params
withSendCoefs scoefs' (Params ss rs sv coefs scoefs rcoefs loops _)
    | dimVector scoefs' /= dimVector scoefs =
        error "dimension mismatch"
    | otherwise = let
        p = Params ss rs sv coefs scoefs' rcoefs loops (senderParamsMap p)
        in p

withReceiveCoefs :: Vector Double -> Params -> Params
withReceiveCoefs rcoefs' (Params ss rs sv coefs scoefs rcoefs loops _)
    | dimVector rcoefs' /= dimVector rcoefs =
        error "dimension mismatch"
    | otherwise = let
        p = Params ss rs sv coefs scoefs rcoefs' loops (senderParamsMap p)
        in p

withSelfLoops :: Bool -> Params -> Params
withSelfLoops loops (Params ss rs sv coefs scoefs rcoefs _ _) = let
    p = Params ss rs sv coefs scoefs rcoefs loops (senderParamsMap p)
    in p

validDyad :: (SenderId, ReceiverId) -> Params -> Bool
validDyad (s,r) p | hasSelfLoops p = True
                  | otherwise      = s /= r

staticWeightSum :: SenderId -> Params -> Double
staticWeightSum s p = case Map.lookup s (senderParams p) of
    Nothing -> error "unknown sender"
    Just sp -> senderStaticWeightSum sp

staticWeight :: (SenderId, ReceiverId) -> Params -> Double
staticWeight sr p = fst $ staticWeightLogPair sr p

staticLogWeight :: (SenderId, ReceiverId) -> Params -> Double
staticLogWeight sr p = snd $ staticWeightLogPair sr p

staticWeightLogPair :: (SenderId, ReceiverId) -> Params -> (Double, Double)
staticWeightLogPair (s,r) p = case Map.lookup s (senderParams p) of
    Nothing -> error "unknown sender"
    Just sp -> case Map.lookup r (senderStaticWeightLog sp) of
        Nothing -> error "invalid or unknown receiver"
        Just w -> w

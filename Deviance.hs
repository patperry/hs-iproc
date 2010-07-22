module Deviance
    where
        
import Control.Arrow( second )
import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Numeric.LinearAlgebra
     
import Actor   
import SVars
import DVars
import Message
import Param
import Summary


data SenderDev =
    SenderDev { param :: !Param
              , staticLogWeight :: !(Map ReceiverId Double)
              , staticWeightSum :: !Double
              , deviance :: !Double
              , staticProbTotal :: !Double
              , dynamicProb :: !(Map ReceiverId Double)
              , sendIWeight :: !(Map IntervalId Double)
              , recvIWeight :: !(Map IntervalId Double)
              , sendIRecvICross :: !(Map (IntervalId,IntervalId) Double)
              }

emptySenderDev :: SVars -> DVars -> Param -> SenderId -> SenderDev
emptySenderDev sv _dv p s = let
    lw = Map.fromList [ (r, (staticCoef p) `dotVector` x)
                      | (r, x) <- svarsOutOf s sv
                      , validDyad p s r
                      ]
    ws = (sum . map exp) $ Map.elems lw
    in SenderDev p lw ws 0 0 Map.empty Map.empty Map.empty Map.empty

updateSenderDev :: SenderDev -> ([ReceiverId], ActorState) -> SenderDev
updateSenderDev (SenderDev p slw sum_w dev s_prob d_prob iw_send iw_recv iw_cross)
                (tos,h) = let
    l = realToFrac $ length tos
    h_send = recentEvents $ sendHistory h
    h_recv = recentEvents $ recvHistory h
    
    dlw = Map.unionsWith (+) $ zipWith coefMap
                [ sendCoef p, recvCoef p] [ h_send, h_recv ]
                
    (sum_w',prob) = Map.mapAccumWithKey (\acc r delta ->
                            let lw = Map.findWithDefault 0 r slw
                                w = exp lw
                                w' = exp (lw + delta)
                                acc' = acc + (w' - w)
                            in acc' `seq` (acc', w' / sum_w')
                        ) sum_w dlw
    wt = Map.map (l*) prob

    dev' = dev - 2 * sum [ log (Map.findWithDefault 0 to prob) | to <- tos ]
    
    s_prob' = s_prob + sum_w / sum_w'
    
    d_prob' = foldl' (\acc (r,lw) ->
                         Map.insertWith' (+) r (exp lw / sum_w') acc)
                     d_prob $ Map.assocs dlw
    
    iw_send' = updateEventWeights wt iw_send h_send
    iw_recv' = updateEventWeights wt iw_recv h_recv
    
    iw_cross' = updateEventWeights wt iw_cross $ Map.assocs $
                    Map.intersectionWith (,)
                        (Map.fromList h_send)
                        (Map.fromList h_recv)

    in SenderDev p slw sum_w dev' s_prob' d_prob' iw_send' iw_recv' iw_cross'
  where
    coefMap coef =
        Map.fromList . map (second $ \int -> atVector coef int)
        
    updateEventWeights wt ew res =
        foldl' (\acc (e, w_r) -> Map.insertWith' (+) e w_r acc)
               ew
               [ (e, Map.findWithDefault 0 r wt) | (r,e) <- res ]

data Deviance =
    Deviance { modelSVars :: !SVars
             , modelDVars :: !DVars
             , modelParams :: !Param
             , senderDev :: !(Map SenderId SenderDev)
             }
             
emptyDeviance :: SVars -> DVars -> Param -> Deviance
emptyDeviance sv dv p =
    Deviance sv dv p Map.empty

updateDeviance :: Deviance -> (Message, DVarsState) -> Deviance
updateDeviance (Deviance sv dv p devs) (Message _ _ from tos, h) = let
    dev  = Map.findWithDefault (emptySenderDev sv dv p from) from devs
    dev' = updateSenderDev dev (tos, dvarsStateOf from h)
    devs' = dev' `seq` Map.insert from dev' devs
    in Deviance sv dv p devs'

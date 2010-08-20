module Vars (
    Vars,
    vars,
    dim,
    senders,
    receivers,

    dyad,
    dyadChanges,
    mulDyadBy,

    sender,
    senderChanges,
    mulSenderBy
    ) where
        
import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( catMaybes, maybeToList )
import Numeric.LinearAlgebra

import Actor
import qualified EventSet as EventSet
import History( History )
import qualified History as History
import Intervals( Intervals, IntervalId )
import qualified Intervals as Intervals

        
data Vars = 
    Vars { dim :: !Int
         , senderIxMap :: !(Map SenderId Int)
         , receiverIxMap :: !(Map ReceiverId Int)
         , senderMatrix :: !(Matrix Double)
         , receiverMatrix :: !(Matrix Double)
         , sendIntervals :: !Intervals
         , receiveIntervals :: !Intervals
         , dynamicDim :: !Int
         , staticMatrixDim :: !(Int, Int)
         } deriving (Eq, Show)
          
vars :: Map SenderId (Vector Double)
     -> Map ReceiverId (Vector Double)
     -> Intervals
     -> Intervals
     -> Vars
vars sm rm sint rint
    | Map.null sm = error "no senders"
    | Map.null rm = error "no receivers"
    | otherwise = let
        sim = Map.fromAscList $ zip (Map.keys sm) [ 0.. ]
        rim = Map.fromAscList $ zip (Map.keys rm) [ 0.. ]
        ssd = (dimVector . snd . Map.findMin) sm
        srd = (dimVector . snd . Map.findMin) rm
        smat = colListMatrix (ssd, Map.size sm) $ Map.elems sm
        rmat = colListMatrix (srd, Map.size rm) $ Map.elems rm
        dd = Intervals.size sint + Intervals.size rint
        smd = (ssd, srd)
        d = dd + ssd * srd
        in Vars d sim rim smat rmat sint rint dd smd
        

senderIndex :: SenderId -> Vars -> Int
senderIndex s v =
    Map.findWithDefault (error $ "unknown sender `" ++ show s ++ "'")
                        s
                        (senderIxMap v)

receiverIndex :: ReceiverId -> Vars -> Int
receiverIndex r v =
    Map.findWithDefault (error $ "unknown receiver `" ++ show r ++ "'")
                        r
                        (receiverIxMap v)

senders :: Vars -> [SenderId]
senders v = Map.keys (senderIxMap v)

receivers :: Vars -> [ReceiverId]
receivers v = Map.keys (receiverIxMap v)

mulSenderBy :: Vector Double -> Vars -> History -> SenderId -> [(ReceiverId, Double)]
mulSenderBy beta v h s
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        (beta0, beta1) = splitVectorAt (dynamicDim v) beta
        beta1_mat = matrixViewVector (staticMatrixDim v) beta1
        
        sx = colMatrix (senderMatrix v) (senderIndex s v)
        sx_beta1 = mulMatrixVector Trans beta1_mat sx
        y0s = mulMatrixVector Trans (receiverMatrix v) sx_beta1
        
        in [ (r, foldl' (+) y0 [ d * atVector beta0 i | (i,d) <- delta r ])
           | (r,y0) <- zip (receivers v) (elemsVector y0s) ]
  where
    delta = dyadChanges v h s


mulDyadBy :: Vector Double -> Vars -> History -> SenderId -> ReceiverId -> Double
mulDyadBy beta v h s r
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        (beta0, beta1) = splitVectorAt (dynamicDim v) beta
        beta1_mat = matrixViewVector (staticMatrixDim v) beta1
        
        sx = colMatrix (senderMatrix v) (senderIndex s v)
        rx = colMatrix (receiverMatrix v) (receiverIndex s v)
        sx_beta1 = mulMatrixVector Trans beta1_mat sx
        y0 = dotVector rx sx_beta1
    
        in foldl' (+) y0 [ d * atVector beta0 i | (i,d) <- delta s r ]
  where
    delta = dyadChanges v h

senderChanges :: Vars -> History -> SenderId -> [(ReceiverId, [(Int,Double)])]
-- senderChanges v h s = [ (r, dyadChanges v h s r) | r <- receivers v ]
senderChanges v h s | History.null h = []
                    | otherwise =
    Map.toList $ Map.unionsWith (++) $ map (Map.fromList . catMaybes)
        [ [ case Intervals.lookup dt sint of
                Nothing -> Nothing
                Just i  -> Just (r, [send i])
          | (r,dt) <- EventSet.past $ History.lookupSender s h
          ]
        , [ case Intervals.lookup dt' rint of
                Nothing -> Nothing
                Just i' -> Just (r, [receive i'])
          | (r,dt') <- EventSet.past $ History.lookupReceiver s h
          ]
        ]
  where
    sint = sendIntervals v
    rint = receiveIntervals v
    send    i  = (i, 1)
    receive i' = (i' + Intervals.size sint, 1)


dyadChanges :: Vars -> History -> SenderId -> ReceiverId -> [(Int,Double)]
dyadChanges v h s r | History.null h = []
                    | otherwise = concatMap maybeToList
    [ do
          dt <- EventSet.lookup r $ History.lookupSender s h
          i <- Intervals.lookup dt sint
          return $ send i
    , do
          dt' <- EventSet.lookup r $ History.lookupReceiver s h
          i' <- Intervals.lookup dt' rint
          return $ receive i'
    ]
  where
    sint = sendIntervals v
    rint = receiveIntervals v
    send    i  = (i, 1)
    receive i' = (i' + Intervals.size sint, 1)


sender :: Vars -> History -> SenderId -> [(ReceiverId, Vector Double)]
sender v h s = [ (r, dyad v h s r) | r <- receivers v ]

dyad :: Vars -> History -> SenderId -> ReceiverId -> Vector Double
dyad v h s r = let
    sx = colMatrix (senderMatrix v) (senderIndex s v)
    rx = colMatrix (receiverMatrix v) (receiverIndex s v)
    delta = dyadChanges v h r s
    in concatVectors [ accumVector (+) (constantVector (dynamicDim v) 0) delta
                     , kroneckerVector rx sx
                     ]

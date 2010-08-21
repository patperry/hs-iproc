module Vars (
    Vars,
    fromActors,
    dim,
    sendIntervals,
    receiveIntervals,
    senders,
    receivers,

    dyad,
    dyadChanges,
    mulDyadBy,
    mulDyadChangesBy,

    sender,
    senderChanges,
    mulSenderBy,
    mulSenderChangesBy,
    
    weightReceiverBy,
    weightReceiverChangesBy,
    ) where

import Control.Monad( forM_ )
import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Set( Set )
import qualified Data.Set as Set
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
         , receiverSet :: !(Set ReceiverId)
         , senderMatrix :: !(Matrix Double)
         , receiverMatrix :: !(Matrix Double)
         , sendIntervals :: !Intervals
         , receiveIntervals :: !Intervals
         , dynamicDim :: !Int
         , staticMatrixDim :: !(Int, Int)
         } deriving (Eq, Show)
          
fromActors :: Map SenderId (Vector Double)
           -> Map ReceiverId (Vector Double)
           -> Intervals
           -> Intervals
           -> Vars
fromActors sm rm sint rint
    | Map.null sm = error "no senders"
    | Map.null rm = error "no receivers"
    | otherwise = let
        sim = Map.fromAscList $ zip (Map.keys sm) [ 0.. ]
        rim = Map.fromAscList $ zip (Map.keys rm) [ 0.. ]
        rs = Map.keysSet rim
        ssd = (dimVector . snd . Map.findMin) sm
        srd = (dimVector . snd . Map.findMin) rm
        smat = colListMatrix (ssd, Map.size sm) $ Map.elems sm
        rmat = colListMatrix (srd, Map.size rm) $ Map.elems rm
        dd = Intervals.size sint + Intervals.size rint
        smd = (ssd, srd)
        d = dd + ssd * srd
        in Vars d sim rim rs smat rmat sint rint dd smd
        

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

senderCount :: Vars -> Int
senderCount = (snd . dimMatrix . senderMatrix)

receiverCount :: Vars -> Int
receiverCount = (snd . dimMatrix . receiverMatrix)

weightReceiverBy :: [(ReceiverId, Double)] -> Vars -> History -> SenderId -> Vector Double
weightReceiverBy rws v h s = let
    w = runVector $ do
            mw <- newVector (receiverCount v) 0
            forM_ rws $ \(r,wt) ->
                modifyVector mw (receiverIndex r v) (wt+)
            return mw
    y = mulMatrixVector NoTrans (receiverMatrix v) w
    x = colMatrix (senderMatrix v) (senderIndex s v)
    in concatVectors 
           [ accumVector (+) (constantVector (dynamicDim v) 0) $
                 [ (i, atVector w (receiverIndex r v) * d)
                 | (r, ds) <- senderChanges v h s
                 , (i, d) <- ds
                 ]
           , kroneckerVector y x
           ]

weightReceiverChangesBy :: [(ReceiverId, Double)] -> Vars -> History -> SenderId -> [(Int, Double)]
weightReceiverChangesBy rws v h s = let
    w = runVector $ do
            mw <- newVector (receiverCount v) 0
            forM_ rws $ \(r,wt) ->
                modifyVector mw (receiverIndex r v) (wt+)
            return mw
    in filter ((/= 0) . snd) $ assocsVector $ 
           accumVector (+) (constantVector (dynamicDim v) 0) $
               [ (i, atVector w (receiverIndex r v) * d)
               | (r, ds) <- senderChanges v h s
               , (i, d) <- ds
               ]

mulSenderBy :: Vector Double -> Vars -> History -> SenderId -> [(ReceiverId, Double)]
mulSenderBy beta v h s
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta1 = dropVector (dynamicDim v) beta
        beta1_mat = matrixViewVector (staticMatrixDim v) beta1
        
        x = colMatrix (senderMatrix v) (senderIndex s v)
        xt_beta1 = mulMatrixVector Trans beta1_mat x
        z = runVector $ do
                 mz <- newVector_ (receiverCount v)
                 mulMatrixToVector Trans (receiverMatrix v) xt_beta1 mz
                 forM_ (mulSenderChangesBy beta v h s) $ \(r,z1) ->
                     unsafeModifyVector mz (receiverIndex r v) (z1+)
                 return mz
        in zip (receivers v) (elemsVector z)

mulSenderChangesBy :: Vector Double -> Vars -> History -> SenderId -> [(ReceiverId, Double)]
mulSenderChangesBy  beta v h s
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta0 = takeVector (dynamicDim v) beta
        in [ (r, foldl' (+) 0 [ d * atVector beta0 i | (i,d) <- delta ])
           | (r, delta) <- senderChanges v h s
           ]

mulDyadBy :: Vector Double -> Vars -> History -> SenderId -> ReceiverId -> Double
mulDyadBy beta v h s r
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta1 = dropVector (dynamicDim v) beta
        beta1_mat = matrixViewVector (staticMatrixDim v) beta1
        
        x = colMatrix (senderMatrix v) (senderIndex s v)
        y = colMatrix (receiverMatrix v) (receiverIndex r v)
        xt_beta1 = mulMatrixVector Trans beta1_mat x
        z0 = dotVector y xt_beta1

        in z0 + mulDyadChangesBy beta v h s r

mulDyadChangesBy :: Vector Double -> Vars -> History -> SenderId -> ReceiverId -> Double
mulDyadChangesBy beta v h s r
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta0 = takeVector (dynamicDim v) beta
        in foldl' (+) 0 [ d * atVector beta0 i | (i,d) <- delta s r ]
  where
    delta = dyadChanges v h

senderChanges :: Vars -> History -> SenderId -> [(ReceiverId, [(Int,Double)])]
-- senderChanges v h s = filter (not . null . snd) [ (r, dyadChanges v h s r) | r <- receivers v ]
senderChanges v h s | History.null h = []
                    | otherwise =
    Map.assocs $ Map.unionsWith (++) $ map (Map.fromList . catMaybes)
        [ [ case Intervals.lookup dt sint of
                Nothing -> Nothing
                Just i  -> Just (r, [send i])
          | (r,dt) <- EventSet.past $ History.lookupSender s h
          ]
        , [ case Intervals.lookup dt' rint of
                Nothing -> Nothing
                Just i' -> Just (r, [receive i'])
          | (r,dt') <- EventSet.past $ History.lookupReceiver s h
          , Set.member r rset 
          ]
        ]
  where
    rset = receiverSet v
    sint = sendIntervals v
    rint = receiveIntervals v
    send    i  = (i, 1)
    receive i' = (i' + Intervals.size sint, 1)


dyadChanges :: Vars -> History -> SenderId -> ReceiverId -> [(Int,Double)]
dyadChanges v h s r | History.null h = []
                    | otherwise = catMaybes
    [ do
          dt <- EventSet.lookup r $ History.lookupSender s h
          i <- Intervals.lookup dt sint
          return $ send i
    , do
          dt' <- EventSet.lookup s $ History.lookupSender r h
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
    x = colMatrix (senderMatrix v) (senderIndex s v)
    y = colMatrix (receiverMatrix v) (receiverIndex r v)
    delta = dyadChanges v h s r
    in concatVectors [ accumVector (+) (constantVector (dynamicDim v) 0) delta
                     , kroneckerVector y x
                     ]

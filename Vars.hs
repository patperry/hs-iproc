module Vars (
    Vars,
    fromActors,
    dim,
    sendIntervals,
    receiveIntervals,

    senders,
    senderCount,
    validSender,

    receivers,
    receiverCount,
    validReceiver,

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
import Data.Maybe( catMaybes )
import Numeric.LinearAlgebra

import History( History )
import Indices( Indices )
import Intervals( Intervals )
import Types
import qualified EventSet as EventSet
import qualified History as History
import qualified Indices as Indices
import qualified Intervals as Intervals



data Vars = 
    Vars { dim :: !Int
         , senderIndices :: !Indices
         , receiverIndices :: !Indices
         , senderMatrix :: !(Matrix Double)
         , receiverMatrix :: !(Matrix Double)
         , sendIntervals :: !Intervals
         , receiveIntervals :: !Intervals
         , dynamicDim :: !Int
         , staticMatrixDim :: !(Int, Int)
         } deriving (Show)
          
fromActors :: Map SenderId (Vector Double)
           -> Map ReceiverId (Vector Double)
           -> Intervals
           -> Intervals
           -> Vars
fromActors sm rm sint rint
    | Map.null sm = error "no senders"
    | Map.null rm = error "no receivers"
    | otherwise = let
        six = Indices.fromAscList $ Map.keys sm
        rix = Indices.fromAscList $ Map.keys rm
        ssd = (dimVector . snd . Map.findMin) sm
        srd = (dimVector . snd . Map.findMin) rm
        smat = colListMatrix (ssd, Map.size sm) $ Map.elems sm
        rmat = colListMatrix (srd, Map.size rm) $ Map.elems rm
        dd = Intervals.size sint + Intervals.size rint
        smd = (ssd, srd)
        d = dd + ssd * srd
        in Vars d six rix smat rmat sint rint dd smd
        
senderCount :: Vars -> Int
senderCount = Indices.size . senderIndices

receiverCount :: Vars -> Int
receiverCount = Indices.size . receiverIndices

senderIndex :: Vars -> SenderId -> Int
senderIndex v s =
    Indices.findWithDefault (error $ "unknown sender `" ++ show s ++ "'")
                            s (senderIndices v)

receiverIndex :: Vars -> ReceiverId -> Int
receiverIndex v r =
    Indices.findWithDefault (error $ "unknown receiver `" ++ show r ++ "'")
                            r (receiverIndices v)

unsafeReceiverIndex :: Vars -> ReceiverId -> Int
unsafeReceiverIndex v r = Indices.unsafeAt (receiverIndices v) r

senders :: Vars -> [SenderId]
senders = Indices.keys . senderIndices

receivers :: Vars -> [ReceiverId]
receivers = Indices.keys . receiverIndices

validReceiver ::  Vars -> ReceiverId -> Bool
validReceiver v r = Indices.member r (receiverIndices v)

validSender ::  Vars -> SenderId -> Bool
validSender v s = Indices.member s (senderIndices v)



weightReceiverBy :: [(ReceiverId, Double)] -> Vars -> History -> SenderId -> Vector Double
weightReceiverBy rws v h s = let
    w = runVector $ do
            mw <- newVector (receiverCount v) 0
            forM_ rws $ \(r,wt) ->
                modifyVector mw (receiverIndex v r) (wt+)
            return mw
    y = mulMatrixVector NoTrans (receiverMatrix v) w
    x = colMatrix (senderMatrix v) (senderIndex v s)
    in concatVectors 
           [ accumVector (+) (constantVector (dynamicDim v) 0) $
                 [ (i, atVector w (unsafeReceiverIndex v r) * d)
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
                unsafeModifyVector mw (receiverIndex v r) (wt+)
            return mw
    
    weight r = unsafeAtVector w (unsafeReceiverIndex v r)
    
    in filter ((/= 0) . snd) $ assocsVector $ 
           accumVector (+) (constantVector (dynamicDim v) 0) $
               [ (i, weight r * d)
               | (r, ds) <- senderChanges v h s
               , (i, d) <- ds
               ]

mulSenderBy :: Vector Double -> Vars -> History -> SenderId -> [(ReceiverId, Double)]
mulSenderBy beta v h s
    | dimVector beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta1 = dropVector (dynamicDim v) beta
        beta1_mat = matrixViewVector (staticMatrixDim v) beta1
        
        x = colMatrix (senderMatrix v) (senderIndex v s)
        xt_beta1 = mulMatrixVector Trans beta1_mat x
        z = runVector $ do
                 mz <- newVector_ (receiverCount v)
                 mulMatrixToVector Trans (receiverMatrix v) xt_beta1 mz
                 forM_ (mulSenderChangesBy beta v h s) $ \(r,z1) ->
                     unsafeModifyVector mz (unsafeReceiverIndex v r) (z1+)
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
        
        x = colMatrix (senderMatrix v) (senderIndex v s)
        y = colMatrix (receiverMatrix v) (receiverIndex v r)
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
senderChanges v h s | not (validSender v s) =
                        error $ "invalid sender: `" ++ show s ++ "'"
                    | History.null h = []
                    | otherwise =
    Map.assocs $ Map.unionsWith (++) $ map (Map.fromList . catMaybes)
        [ [ case Intervals.lookup dt sint of
                Nothing -> Nothing
                Just i  -> Just (r, [send i])
          | (r,dt) <- EventSet.past $ History.lookupSender s h
          , validReceiver v r
          ]
        , [ case Intervals.lookup dt' rint of
                Nothing -> Nothing
                Just i' -> Just (r, [receive i'])
          | (r,dt') <- EventSet.past $ History.lookupReceiver s h
          , validReceiver v r
          ]
        ]
  where
    sint = sendIntervals v
    rint = receiveIntervals v
    send    i  = (i, 1)
    receive i' = (i' + Intervals.size sint, 1)


dyadChanges :: Vars -> History -> SenderId -> ReceiverId -> [(Int,Double)]
dyadChanges v h s r | not (validSender v s) =
                        error $ "invalid sender: `" ++ show s ++ "'"
                    | not (validReceiver v r) =
                        error $ "invalid receiver: `" ++ show r ++ "'"
                    | History.null h = []
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
    x = colMatrix (senderMatrix v) (senderIndex v s)
    y = colMatrix (receiverMatrix v) (receiverIndex v r)
    delta = dyadChanges v h s r
    in concatVectors [ accumVector (+) (constantVector (dynamicDim v) 0) delta
                     , kroneckerVector y x
                     ]

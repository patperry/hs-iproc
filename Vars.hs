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
import qualified Numeric.LinearAlgebra.Matrix as M
import qualified Numeric.LinearAlgebra.Vector as V

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
        ssd = (V.dim . snd . Map.findMin) sm
        srd = (V.dim . snd . Map.findMin) rm
        smat = M.fromCols (ssd, Map.size sm) $ Map.elems sm
        rmat = M.fromCols (srd, Map.size rm) $ Map.elems rm
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
    w = V.create $ do
            mw <- V.new (receiverCount v) 0
            forM_ rws $ \(r,wt) ->
                V.modify mw (receiverIndex v r) (wt+)
            return mw
    y = M.mulVector NoTrans (receiverMatrix v) w
    x = M.col (senderMatrix v) (senderIndex v s)
    in V.concat
           [ V.accum (+) (V.constant (dynamicDim v) 0) $
                 [ (i, V.at w (unsafeReceiverIndex v r) * d)
                 | (r, ds) <- senderChanges v h s
                 , (i, d) <- ds
                 ]
           , V.kronecker y x
           ]

weightReceiverChangesBy :: [(ReceiverId, Double)] -> Vars -> History -> SenderId -> [(Int, Double)]
weightReceiverChangesBy rws v h s = let
    w = V.create $ do
            mw <- V.new (receiverCount v) 0
            forM_ rws $ \(r,wt) ->
                V.unsafeModify mw (receiverIndex v r) (wt+)
            return mw
    
    weight r = V.unsafeAt w (unsafeReceiverIndex v r)
    
    in filter ((/= 0) . snd) $ V.assocs $ 
           V.accum (+) (V.constant (dynamicDim v) 0) $
               [ (i, weight r * d)
               | (r, ds) <- senderChanges v h s
               , (i, d) <- ds
               ]

mulSenderBy :: Vector Double -> Vars -> History -> SenderId -> [(ReceiverId, Double)]
mulSenderBy beta v h s
    | V.dim beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta1 = V.drop (dynamicDim v) beta
        beta1_mat = M.fromVector (staticMatrixDim v) beta1
        
        x = M.col (senderMatrix v) (senderIndex v s)
        xt_beta1 = M.mulVector Trans beta1_mat x
        z = V.create $ do
                 mz <- V.new_ (receiverCount v)
                 M.mulVectorTo mz Trans (receiverMatrix v) xt_beta1
                 forM_ (mulSenderChangesBy beta v h s) $ \(r,z1) ->
                     V.unsafeModify mz (unsafeReceiverIndex v r) (z1+)
                 return mz
        in zip (receivers v) (V.elems z)

mulSenderChangesBy :: Vector Double -> Vars -> History -> SenderId -> [(ReceiverId, Double)]
mulSenderChangesBy  beta v h s
    | V.dim beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta0 = V.take (dynamicDim v) beta
        in [ (r, foldl' (+) 0 [ d * V.at beta0 i | (i,d) <- delta ])
           | (r, delta) <- senderChanges v h s
           ]

mulDyadBy :: Vector Double -> Vars -> History -> SenderId -> ReceiverId -> Double
mulDyadBy beta v h s r
    | V.dim beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta1 = V.drop (dynamicDim v) beta
        beta1_mat = M.fromVector (staticMatrixDim v) beta1
        
        x = M.col (senderMatrix v) (senderIndex v s)
        y = M.col (receiverMatrix v) (receiverIndex v r)
        xt_beta1 = M.mulVector Trans beta1_mat x
        z0 = V.dot y xt_beta1

        in z0 + mulDyadChangesBy beta v h s r

mulDyadChangesBy :: Vector Double -> Vars -> History -> SenderId -> ReceiverId -> Double
mulDyadChangesBy beta v h s r
    | V.dim beta /= dim v = error "dimension mismatch"
    | otherwise = let
        beta0 = V.take (dynamicDim v) beta
        in foldl' (+) 0 [ d * V.at beta0 i | (i,d) <- delta s r ]
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
    x = M.col (senderMatrix v) (senderIndex v s)
    y = M.col (receiverMatrix v) (receiverIndex v r)
    delta = dyadChanges v h s r
    in V.concat [ V.accum (+) (V.constant (dynamicDim v) 0) delta
                , V.kronecker y x
                ]

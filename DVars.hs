module DVars (
    DVars( sendIntervals, receiveIntervals, time ),
    empty,
    insert,
    advanceTo,
    advanceBy,
    lookupSender,
    lookupDyad,
    senderHistory,
    receiverHistory,
    
    DVar(..),
    sendIntervalId,
    receiveIntervalId,
    
    ) where
        
import Control.Arrow( second )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Time

import Actor
import IntervalSet( IntervalSet, IntervalId )
import qualified IntervalSet as IntervalSet
import History( History )
import qualified History as History

data ActorHistory = ActorHistory !(History ReceiverId) !(History SenderId)

data DVars =
    DVars { sendIntervals :: !IntervalSet
          , receiveIntervals :: !IntervalSet
          , time :: !UTCTime
          , senderHistoryMap :: !(Map SenderId (History ReceiverId))
          , receiverHistoryMap :: !(Map ReceiverId (History SenderId))
          } deriving (Eq, Show)

empty :: IntervalSet -> IntervalSet -> UTCTime -> DVars
empty sint rint t0 = DVars sint rint t0 Map.empty Map.empty
          
advanceTo :: UTCTime -> DVars -> DVars
advanceTo t' dvars@(DVars sint rint t shm rhm) | t' < t = error "negative time difference"
                                               | t' == t = dvars
                                               | otherwise =
    DVars sint rint t' shm rhm

advanceBy :: DiffTime -> DVars -> DVars
advanceBy dt dvars | dt == 0 = dvars
                   | dt < 0 = error "negative time difference"
                   | otherwise = let
    t = realToFrac dt `addUTCTime` time dvars
    in advanceTo t dvars

insert :: (SenderId, [ReceiverId]) -> DVars -> DVars
insert (s,rs) (DVars sint rint t shm rhm) = let
    shm' = updateHistoryMap sint (s,rs) shm
    rhm' = foldr (updateHistoryMap rint) rhm (zip rs $ repeat [s])
    in DVars sint rint t shm' rhm'
  where
    updateHistoryMap int (x,ys) hm = let
        h = History.advanceTo t $
                 Map.findWithDefault (History.empty int t) x hm
        h' = foldr History.insert h ys
        in Map.insert x h' hm
    
senderHistory :: SenderId -> DVars -> History ReceiverId
senderHistory s (DVars sint _ t shm _) = 
    History.advanceTo t $
        Map.findWithDefault (History.empty sint t) s shm

receiverHistory :: ReceiverId -> DVars -> History SenderId
receiverHistory r (DVars _ rint t _ rhm) = 
    History.advanceTo t $
        Map.findWithDefault (History.empty rint t) r rhm

data DVar = Send !IntervalId
          | Receive !IntervalId
          | SendAndReceive !IntervalId !IntervalId
    deriving (Eq, Show)

sendIntervalId :: DVar -> Maybe IntervalId
sendIntervalId dvar = case dvar of
    Send i -> Just i
    Receive _ -> Nothing
    SendAndReceive i _ -> Just i

receiveIntervalId :: DVar -> Maybe IntervalId
receiveIntervalId dvar = case dvar of
    Send _ -> Nothing
    Receive i -> Just i
    SendAndReceive _ i -> Just i

lookupSender :: SenderId -> DVars -> [(ReceiverId, DVar)]
lookupSender s dvars = let
    m = Map.fromList $ map (second Send) $ History.pastEvents $ senderHistory s dvars
    m' = foldr (uncurry $ Map.insertWith' (\(Receive j) (Send i) -> SendAndReceive i j))
               m
               (map (second Receive) $ History.pastEvents $ receiverHistory s dvars)
    in Map.toList m'

lookupDyad :: (SenderId, ReceiverId) -> DVars -> Maybe DVar
lookupDyad (s,r) dvars = let
    mi = History.lookup r $ senderHistory s dvars
    mj = History.lookup r $ receiverHistory s dvars
    in case (mi,mj) of
        (Just i, Just j) -> Just $ SendAndReceive i j
        (Just i, Nothing) -> Just $ Send i
        (Nothing, Just j) -> Just $ Receive j
        (Nothing, Nothing) -> Nothing

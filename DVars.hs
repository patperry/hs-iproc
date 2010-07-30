module DVars (
    DVars( sendIntervals, receiveIntervals, time ),
    empty,
    insert,
    advanceTo,
    advanceBy,
    accum,
    lookupSender,
    lookupDyad,
    senderHistory,
    receiverHistory,
    
    DVar(..),
    send,
    receive,
    sendReceive,
    
    ) where
        
import Control.Arrow( second )
import Data.List( mapAccumL )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Time

import Actor
import Intervals( Intervals, IntervalId )
import History( History )
import qualified History as History
import Message


data DVars =
    DVars { sendIntervals :: !Intervals
          , receiveIntervals :: !Intervals
          , time :: !UTCTime
          , senderHistoryMap :: !(Map SenderId (History ReceiverId))
          , receiverHistoryMap :: !(Map ReceiverId (History SenderId))
          } deriving (Eq, Show)

empty :: Intervals -> Intervals -> UTCTime -> DVars
empty sint rint t0 = DVars sint rint t0 Map.empty Map.empty
          
advanceTo :: UTCTime -> DVars -> DVars
advanceTo t' dvars@(DVars sint rint t shm rhm) | t' < t = error "negative time difference"
                                               | t' == t = dvars
                                               | otherwise =
    DVars sint rint t' shm rhm

advanceBy :: NominalDiffTime -> DVars -> DVars
advanceBy dt dvars | dt == 0 = dvars
                   | dt < 0 = error "negative time difference"
                   | otherwise = let
    t = dt `addUTCTime` time dvars
    in advanceTo t dvars

insert :: Message -> DVars -> DVars
insert (Message s rs) (DVars sint rint t shm rhm) = let
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

send :: DVar -> Maybe IntervalId
send dvar = case dvar of
    Send i -> Just i
    Receive _ -> Nothing
    SendAndReceive i _ -> Just i

receive :: DVar -> Maybe IntervalId
receive dvar = case dvar of
    Send _ -> Nothing
    Receive i' -> Just i'
    SendAndReceive _ i' -> Just i'
    
sendReceive :: DVar -> Maybe (IntervalId, IntervalId)
sendReceive dvar = case dvar of
    Send _ -> Nothing
    Receive _ -> Nothing
    SendAndReceive i i' -> Just (i,i')

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

accum :: DVars -> [(UTCTime, Message)] -> (DVars, [(DVars, Message)])
accum =
    mapAccumL (\d0 (t,m) -> 
            let d  = advanceTo t d0
                d' = insert m d
            in (d', (d,m)))

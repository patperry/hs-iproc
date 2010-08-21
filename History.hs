module History (
    History,
    currentTime,
    
    null,
    empty,
    insert,
    advanceTo,
    advanceBy,

    senders,
    receivers,
    lookupSender,
    lookupReceiver,
    
    accum,

    ) where

import Prelude hiding ( null )

import Data.List( foldl', mapAccumL )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Time

import Actor
import EventSet( EventSet )
import qualified EventSet as EventSet
import Message


data History =
    History { currentTime :: !UTCTime
            , senderMap :: !(Map SenderId (EventSet ReceiverId))
            , receiverMap :: !(Map ReceiverId (EventSet SenderId))
            }
    deriving (Eq, Show)

null :: History -> Bool
null (History _ sm rm) = Map.null sm && Map.null rm
    
empty :: UTCTime -> History
empty t0 = History t0 Map.empty Map.empty
          
advanceTo :: UTCTime -> History -> History
advanceTo t' h | t' < t = error "negative time difference"
               | t' == t = h
               | otherwise = h{ currentTime = t' }
  where
    t = currentTime h

advanceBy :: NominalDiffTime -> History -> History
advanceBy dt h | dt == 0 = h
               | dt < 0 = error "negative time difference"
               | otherwise = advanceTo (dt `addUTCTime` currentTime h) h

insert :: Message -> History -> History
insert (Message s rs) (History t sm rm) = let
    sm' = updateEvents (s,rs) sm
    rm' = foldl' (flip updateEvents) rm (zip rs $ repeat [s])
    in History t sm' rm'
  where
    updateEvents (x,ys) m = let
        es  = EventSet.advanceTo t $
                  Map.findWithDefault (EventSet.empty t) x m
        es' = foldl' (flip EventSet.insert) es ys
        in Map.insert x es' m

senders :: History -> [SenderId]
senders (History _ sm _) = Map.keys sm

receivers :: History -> [ReceiverId]
receivers (History _ _ rm) = Map.keys rm

lookupSender :: SenderId -> History -> EventSet ReceiverId
lookupSender s (History t sm _) = 
    EventSet.advanceTo t $
        Map.findWithDefault (EventSet.empty t) s sm

lookupReceiver :: ReceiverId -> History -> EventSet SenderId
lookupReceiver r (History t _ rm) = 
    EventSet.advanceTo t $
        Map.findWithDefault (EventSet.empty t) r rm

accum :: History -> [(UTCTime, Message)] -> (History, [(History, Message)])
accum =
    mapAccumL (\h0 (t,m) -> 
            let h  = advanceTo t h0
                h' = insert m h
            in (h', (h,m)))

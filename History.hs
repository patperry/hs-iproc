module History (
    History,
    null,
    empty,

    senders,
    receivers,
    lookupSender,
    lookupReceiver,
    
    insert,
    advance,
    accum,

    ) where

import Prelude hiding ( null )

import Data.List( foldl', mapAccumL )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( fromMaybe )
import Data.Time

import Actor
import EventSet( EventSet )
import qualified EventSet as EventSet
import Message

type Time = NominalDiffTime

data History =
    History { currentTime :: !Time
            , senderMap :: !(Map SenderId (Time, EventSet ReceiverId))
            , receiverMap :: !(Map ReceiverId (Time, EventSet SenderId))
            }
    deriving (Eq, Show)

null :: History -> Bool
null (History t sm rm) = t == 0 || (Map.null sm && Map.null rm)
    
empty :: History
empty = History 0 Map.empty Map.empty
          
advance :: NominalDiffTime -> History -> History
advance dt h | dt == 0 = h
             | dt < 0 = error "negative time difference"
             | otherwise = h{ currentTime = currentTime h + dt }

insert :: Message -> History -> History
insert (Message s rs) (History t sm rm) = let
    sm' = updateEvents (s,rs) sm
    rm' = foldl' (flip updateEvents) rm (zip rs $ repeat [s])
    in History t sm' rm'
  where
    updateEvents (x,ys) m = let
        (t0,es) = Map.findWithDefault (t, EventSet.empty) x m
        es' = foldl' (flip EventSet.insert) (EventSet.advance (t-t0) es) ys
        in es' `seq` Map.insert x (t,es') m

senders :: History -> [SenderId]
senders h = Map.keys $ senderMap h

receivers :: History -> [ReceiverId]
receivers h = Map.keys $ receiverMap h

lookupSender :: SenderId -> History -> EventSet ReceiverId
lookupSender s (History t sm _) =
    fromMaybe EventSet.empty $ do
         (t0,es0) <- Map.lookup s sm
         return $ EventSet.advance (t-t0) es0

lookupReceiver :: ReceiverId -> History -> EventSet SenderId
lookupReceiver r (History t _ rm) =
    fromMaybe EventSet.empty $ do
        (t0,es0) <- Map.lookup r rm
        return $ EventSet.advance (t-t0) es0

accum :: (UTCTime, History)
      -> [(UTCTime, Message)]
      -> ((UTCTime, History), [(UTCTime, Message, History)])
accum =
    mapAccumL (\(t0,h0) (t,m) -> 
            let h  = advance (t `diffUTCTime` t0) h0
                h' = insert m h
            in ((t,h'), (t,m,h)))

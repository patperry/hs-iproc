module Context (
    Context,
    time,
    
    empty,
    insert,
    advanceTo,
    advanceBy,

    senders,
    receivers,
    senderHistory,
    receiverHistory,
    
    accum,

    ) where

import Data.List( foldl', mapAccumL )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Time

import Actor
import Intervals( Intervals, IntervalId )
import History( History )
import qualified History as History
import Message


data Context =
    Context { time :: !UTCTime
            , senderHistoryMap :: !(Map SenderId (History ReceiverId))
            , receiverHistoryMap :: !(Map ReceiverId (History SenderId))
            }
    deriving (Eq, Show)
    
empty :: UTCTime -> Context
empty t0 = Context t0 Map.empty Map.empty
          
advanceTo :: UTCTime -> Context -> Context
advanceTo t' c | t' < t = error "negative time difference"
               | t' == t = c
               | otherwise = c{ time = t' }
  where
    t = time c

advanceBy :: NominalDiffTime -> Context -> Context
advanceBy dt c | dt == 0 = c
               | dt < 0 = error "negative time difference"
               | otherwise = advanceTo (dt `addUTCTime` time c) c

insert :: Message -> Context -> Context
insert (Message s rs) (Context t shm rhm) = let
    shm' = updateHistoryMap (s,rs) shm
    rhm' = foldl' (flip updateHistoryMap) rhm (zip rs $ repeat [s])
    in Context t shm' rhm'
  where
    updateHistoryMap (x,ys) hm = let
        h = History.advanceTo t $
                 Map.findWithDefault (History.empty t) x hm
        h' = foldl' (flip History.insert) h ys
        in Map.insert x h' hm

senders :: Context -> [SenderId]
senders (Context _ shm _) = Map.keys shm

receivers :: Context -> [ReceiverId]
receivers (Context _ _ rhm) = Map.keys rhm

senderHistory :: SenderId -> Context -> History ReceiverId
senderHistory s (Context t shm _) = 
    History.advanceTo t $
        Map.findWithDefault (History.empty t) s shm

receiverHistory :: ReceiverId -> Context -> History SenderId
receiverHistory r (Context t _ rhm) = 
    History.advanceTo t $
        Map.findWithDefault (History.empty t) r rhm

accum :: Context -> [(UTCTime, Message)] -> (Context, [(Context, Message)])
accum =
    mapAccumL (\d0 (t,m) -> 
            let d  = advanceTo t d0
                d' = insert m d
            in (d', (d,m)))

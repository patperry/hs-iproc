module History (
    History( intervalSet, time ),
    empty,
    insert,
    lookup,
    
    currentEvents,
    pastEvents,
    pastEventsWithTimes,
    
    advanceTo,
    advanceBy,
    
    ) where

import Prelude hiding ( lookup )

import Data.List( sort )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( listToMaybe )
import Data.Set( Set )
import qualified Data.Set as Set
import Data.Time

import IntervalSet( IntervalSet, IntervalId )
import qualified IntervalSet as IntervalSet


data EventDiffTime = EventDiffTime !IntervalId !DiffTime deriving (Eq, Show)

data History e = 
    History { intervalSet :: !IntervalSet
            , time :: !UTCTime
            , pastEventMap :: !(Map e EventDiffTime)
            , currentEventSet :: !(Set e)
            } deriving (Eq, Show)

empty :: IntervalSet -> UTCTime -> History e
empty iset t0 = History iset t0 Map.empty Set.empty

insert :: (Ord e) => e -> History e -> History e
insert e (History iset t past cur) = let
    cur' = Set.insert e cur
    in History iset t past cur'

lookup :: (Ord e) => e -> History e -> Maybe IntervalId
lookup e h = unEventDiffTime `fmap` Map.lookup e (pastEventMap h)
  where
    unEventDiffTime (EventDiffTime i _) = i

currentEvents :: History e -> [e]
currentEvents = Set.elems . currentEventSet

pastEvents :: History e -> [(IntervalId, e)]
pastEvents = map unEventDiffTime . Map.assocs . pastEventMap
  where
    unEventDiffTime (e, EventDiffTime i _) = (i, e)

pastEventsWithTimes :: History e -> [(DiffTime, e)]
pastEventsWithTimes = map unEventDiffTime . Map.assocs . pastEventMap
  where
    unEventDiffTime (e, EventDiffTime _ t) = (t, e)

advanceTo :: (Ord e) => UTCTime -> History e -> History e
advanceTo t h@(History iset t0 past cur) | t == t0 = h
                                         | t < t0 = error "negative time difference"
                                         | otherwise = let
    dt = realToFrac $ t `diffUTCTime` t0
    iset_assocs = IntervalSet.assocs iset
    past' = flip Map.mapMaybe past $ \(EventDiffTime i d) ->
                let d'  = d + dt
                    id' = listToMaybe [ EventDiffTime int_id d'
                                      | (int_id, int) <- drop i iset_assocs
                                      , d' <= int ]
                in id'
    past'' = case IntervalSet.lookup dt iset of
                 Nothing -> past'
                 Just i0 ->
                    Set.fold (\e -> Map.insert e $ EventDiffTime i0 dt)
                             past'
                             cur
                             
    in History iset t past'' Set.empty

advanceBy :: (Ord e) => DiffTime -> History e -> History e
advanceBy dt h | dt == 0 = h
               | dt < 0 = error "negative time difference"
               | otherwise = let
    t = realToFrac dt `addUTCTime` time h
    in advanceTo t h

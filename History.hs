module History (
    History( time ),
    empty,
    insert,
    lookup,
    
    currentEvents,
    pastEvents,
    
    advanceTo,
    advanceBy,
    
    ) where

import Prelude hiding ( lookup )

import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( listToMaybe )
import Data.Set( Set )
import qualified Data.Set as Set
import Data.Time

data History e = 
    History { time :: !UTCTime
            , pastEventMap :: !(Map e NominalDiffTime)
            , currentEventSet :: !(Set e)
            } deriving (Eq, Show)

empty :: UTCTime -> History e
empty t0 = History t0 Map.empty Set.empty

insert :: (Ord e) => e -> History e -> History e
insert e (History t past cur) = let
    cur' = Set.insert e cur
    in History t past cur'

lookup :: (Ord e) => e -> History e -> Maybe NominalDiffTime
lookup e h = Map.lookup e (pastEventMap h)

currentEvents :: History e -> [e]
currentEvents = Set.elems . currentEventSet

pastEvents :: History e -> [(e, NominalDiffTime)]
pastEvents = Map.assocs . pastEventMap

advanceTo :: (Ord e) => UTCTime -> History e -> History e
advanceTo t h@(History t0 past cur) | t == t0 = h
                                    | t < t0 = error "negative time difference"
                                    | otherwise = let
    dt = t `diffUTCTime` t0
    past' = Map.map (dt+) past
    past'' = Set.fold (`Map.insert` dt) past' cur
    in History t past'' Set.empty

advanceBy :: (Ord e) => NominalDiffTime -> History e -> History e
advanceBy dt h | dt == 0 = h
               | dt < 0 = error "negative time difference"
               | otherwise = let
    t = dt `addUTCTime` time h
    in advanceTo t h

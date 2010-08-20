module EventSet (
    EventSet( currentTime ),
    empty,
    insert,
    lookup,
    
    current,
    past,
    
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

data EventSet e = 
    EventSet { currentTime :: !UTCTime
             , pastEventMap :: !(Map e NominalDiffTime)
             , currentEventSet :: !(Set e)
             } deriving (Eq, Show)

empty :: UTCTime -> EventSet e
empty t0 = EventSet t0 Map.empty Set.empty

insert :: (Ord e) => e -> EventSet e -> EventSet e
insert e (EventSet t past cur) = let
    cur' = Set.insert e cur
    in EventSet t past cur'

lookup :: (Ord e) => e -> EventSet e -> Maybe NominalDiffTime
lookup e h = Map.lookup e (pastEventMap h)

current :: EventSet e -> [e]
current = Set.elems . currentEventSet

past :: EventSet e -> [(e, NominalDiffTime)]
past = Map.assocs . pastEventMap

advanceTo :: (Ord e) => UTCTime -> EventSet e -> EventSet e
advanceTo t es@(EventSet t0 past cur) | t == t0 = es
                                      | t < t0 = error "negative time difference"
                                      | otherwise = let
    dt = t `diffUTCTime` t0
    past' = Map.map (dt+) past
    past'' = Set.fold (`Map.insert` dt) past' cur
    in EventSet t past'' Set.empty

advanceBy :: (Ord e) => NominalDiffTime -> EventSet e -> EventSet e
advanceBy dt es | dt == 0 = es
                | dt < 0 = error "negative time difference"
                | otherwise = let
    t = dt `addUTCTime` currentTime es
    in advanceTo t es

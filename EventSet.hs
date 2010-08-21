module EventSet (
    EventSet,
    null,
    empty,
    lookup,
    
    current,
    past,
    
    insert,
    advance,
    
    ) where

import Prelude hiding ( lookup, null )

import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( listToMaybe )
import Data.Set( Set )
import qualified Data.Set as Set
import Data.Time

data EventSet e = 
    EventSet { pastEventMap :: !(Map e NominalDiffTime)
             , currentEventSet :: !(Set e)
             } deriving (Eq, Show)

null :: EventSet e -> Bool
null (EventSet p c) = Map.null p && Set.null c

empty :: EventSet e
empty = EventSet Map.empty Set.empty

insert :: (Ord e) => e -> EventSet e -> EventSet e
insert e (EventSet past cur) = let
    cur' = Set.insert e cur
    in EventSet past cur'

lookup :: (Ord e) => e -> EventSet e -> Maybe NominalDiffTime
lookup e h = Map.lookup e (pastEventMap h)

current :: EventSet e -> [e]
current = Set.elems . currentEventSet

past :: EventSet e -> [(e, NominalDiffTime)]
past = Map.assocs . pastEventMap

advance :: (Ord e) => NominalDiffTime -> EventSet e -> EventSet e
advance dt es@(EventSet past cur) | dt == 0 = es
                                  | dt < 0 = error "negative time difference"
                                  | otherwise = let
    past' = Map.map (dt+) past
    past'' = Set.fold (`Map.insert` dt) past' cur
    in EventSet past'' Set.empty

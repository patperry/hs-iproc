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
import Data.Set( Set )
import qualified Data.Set as Set
import Types( DiffTime )

data EventSet e = 
    EventSet { pastEventMap :: !(Map e DiffTime)
             , currentEventSet :: !(Set e)
             } deriving (Eq, Show)

null :: EventSet e -> Bool
null (EventSet p c) = Map.null p && Set.null c

empty :: EventSet e
empty = EventSet Map.empty Set.empty

insert :: (Ord e) => e -> EventSet e -> EventSet e
insert e (EventSet p c) = let
    c' = Set.insert e c
    in EventSet p c'

lookup :: (Ord e) => e -> EventSet e -> Maybe DiffTime
lookup e h = Map.lookup e (pastEventMap h)

current :: EventSet e -> [e]
current = Set.elems . currentEventSet

past :: EventSet e -> [(e, DiffTime)]
past = Map.assocs . pastEventMap

advance :: (Ord e) => DiffTime -> EventSet e -> EventSet e
advance dt es@(EventSet p c) | dt == 0 = es
                             | dt < 0 = error "negative time difference"
                             | otherwise = let
    p' = Map.map (dt+) p
    p'' = Set.fold (`Map.insert` dt) p' c
    in EventSet p'' Set.empty

module ActorSet (
    ActorSet,
    SenderSet,
    ReceiverSet,
    
    fromList,
    lookup,
    at,
    size,
    assocs,
    toList,
    ) where
    
import Prelude hiding ( lookup )
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Data.Maybe( fromJust )

import Actor

data ActorSet = ActorSet !Int !(IntMap Actor) !(IntMap Int) deriving (Eq,Show)

type SenderSet = ActorSet
type ReceiverSet = ActorSet


fromList :: [Actor] -> ActorSet
fromList as = ActorSet (length as)
                       (IntMap.fromAscList $ zip [ 0.. ] as)
                       (IntMap.fromList $ zip (map actorId as) [ 0.. ])

lookup :: ActorId -> ActorSet -> Maybe Int
lookup aid (ActorSet _ _ idx) = IntMap.lookup aid idx

at :: Int -> ActorSet -> Actor
at i (ActorSet n as _) | i < 0 = error "negative index"
                       | i >= n = error "index too large"
                       | otherwise =
    fromJust $ IntMap.lookup i as

size :: ActorSet -> Int
size (ActorSet n _ _) = n

assocs :: ActorSet -> [(Int,Actor)]
assocs (ActorSet _ as _) = IntMap.assocs as

toList :: ActorSet -> [Actor]
toList (ActorSet _ as _) = IntMap.elems as

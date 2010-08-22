module Indices (
    Indices,
    Key,
    
    fromList,
    fromAscList,
    
    size,
    (!),
    lookup,
    findWithDefault,
    keys,
    member,
    ) where

import Prelude hiding ( lookup )

import Data.List( sort )
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Data.IntSet( IntSet )
import qualified Data.IntSet as IntSet


type Key = Int

data Indices =
    Indices { ixMap :: !(IntMap Int)
            , ixSet :: !IntSet
            , size :: !Int
            }
    deriving (Show)


fromList :: [Key] -> Indices
fromList is = fromAscList (sort is)

fromAscList :: [Key] -> Indices
fromAscList is = let
    m = IntMap.fromAscList $ zip is [ 0.. ]
    s = IntMap.keysSet m
    c = IntSet.size s
    in Indices { ixMap = m, ixSet = s, size = c }

(!) :: Indices -> Key -> Int
(!) (Indices m _ _) k = (IntMap.!) m k

lookup :: Key -> Indices -> Maybe Int
lookup k (Indices m _ _) = IntMap.lookup k m

findWithDefault :: Int -> Key -> Indices -> Int
findWithDefault i0 k (Indices m _ _) = IntMap.findWithDefault i0 k m

keys :: Indices -> [Key]
keys (Indices _ s _) = IntSet.elems s

member :: Key -> Indices -> Bool
member i (Indices _ s _) = IntSet.member i s

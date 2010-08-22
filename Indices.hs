module Indices (
    Indices,
    Key,
    
    fromList,
    fromAscList,
    
    size,
    (!),
    unsafeAt,
    lookup,
    findWithDefault,
    keys,
    member,
    ) where

import Prelude hiding ( lookup )

import Data.List( sort )
import Data.Maybe( fromMaybe )
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Data.IntSet( IntSet )
import qualified Data.IntSet as IntSet


type Key = Int

data Indices =
      Ordered { lower :: !Int
              , upper :: !Int
              , size :: !Int
              }
    | Indices { ixMap :: !(IntMap Int)
              , ixSet :: !IntSet
              , size :: !Int
              }
    deriving (Show)


fromList :: [Key] -> Indices
fromList is = fromAscList (sort is)

fromAscList :: [Key] -> Indices
fromAscList [] = Ordered 0 0 0
fromAscList is | sequential is = Ordered { lower = (head is)
                                         , upper = (1 + last is)
                                         , size = (length is)
                                         }
               | otherwise = let
    m = IntMap.fromAscList $ zip is [ 0.. ]
    s = IntMap.keysSet m
    c = IntSet.size s
    in Indices { ixMap = m, ixSet = s, size = c }
  where
    sequential [] = True
    sequential xs = xs == [ head xs..last xs ]

(!) :: Indices -> Key -> Int
(!) (Indices m _ _) k = (IntMap.!) m k
(!) is k = fromMaybe (error "Indices: invalid key") $ lookup k is

unsafeAt :: Indices -> Key -> Int
unsafeAt (Ordered l _ _) k = k - l
unsafeAt is k = (!) is k

lookup :: Key -> Indices -> Maybe Int
lookup k (Indices m _ _) = IntMap.lookup k m
lookup k (Ordered l u _) | l <= k && k < u = Just (k-l)
                         | otherwise = Nothing

findWithDefault :: Int -> Key -> Indices -> Int
findWithDefault i0 k (Indices m _ _) = IntMap.findWithDefault i0 k m
findWithDefault i0 k is = fromMaybe i0 $ lookup k is

keys :: Indices -> [Key]
keys (Indices _ s _) = IntSet.elems s
keys (Ordered l u _) = [ l..(u-1) ]

member :: Key -> Indices -> Bool
member k (Indices _ s _) = IntSet.member k s
member k (Ordered l u _) = l <= k && k < u

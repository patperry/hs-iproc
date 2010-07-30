module Summary (
    Summary(..),
    fromList,
    empty,
    singleton,
    insert,
    union,
    ) where

import Data.List( foldl', foldl1' )
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor
import DVars( DVars, Context )
import qualified DVars as DVars
import Intervals( IntervalId )
import SVars( SVars )
import Message
import qualified SVars as SVars

        
data Summary = 
    Summary { svars :: !SVars
            , dvars :: !DVars
            , messageCount :: !Int
            , messageLengthCount :: !(Map Int Int)
            , sendCount :: !(Map SenderId Int)
            , receiveCount :: !(Map ReceiverId Int)
            , svarsSum :: !(Vector Double)
            , dvarsSendSum :: !(Map IntervalId Int)
            , dvarsReceiveSum :: !(Map IntervalId Int)
            } deriving (Eq, Show)


fromList :: SVars -> DVars -> [(Context, Message)] -> Summary
fromList sv dv ms = let
    in foldl' (flip insert) (empty sv dv) ms
        
empty :: SVars -> DVars -> Summary
empty sv dv =
    let x0 = constantVector (SVars.dim sv) 0
    in Summary sv dv 0 Map.empty Map.empty Map.empty x0 Map.empty Map.empty

singleton :: SVars -> DVars -> (Context, Message) -> Summary
singleton sv dv = flip insert (empty sv dv)

union :: Summary -> Summary -> Summary
union (Summary sv1 dv1 n1 l1 s1 r1 x1 si1 ri1)
      (Summary _sv2 _dv2 n2 l2 s2 r2 x2 si2 ri2) = let
      sv = sv1
      dv = dv1
      n = n1 + n2
      l = unionWith' (+) l1 l2
      s = unionWith' (+) s1 s2
      r = unionWith' (+) r1 r2
      x = addVector x1 x2
      si = unionWith' (+) si1 si2
      ri = unionWith' (+) ri1 ri2
      in Summary sv dv n l s r x si ri
  where
    unionWith' f m m' =
        foldl' (flip $ uncurry $ Map.insertWith' (flip f))
               m
               (Map.toList m')

insert :: (Context, Message) -> Summary -> Summary
insert (c,m) (Summary sv dv n l s r x si ri) = let
    n' = n + 1
    l' = Map.insertWith' (+) (length ts) 1 l
    s' = Map.insertWith' (+) f (length ts) s
    r' = foldl' (flip $ \t -> Map.insertWith' (+) t 1) r ts
    x' = addVector x $
             foldl1' addVector [ SVars.lookupDyad (f,t) sv | t <- ts ]
    si' = foldl' (flip $ \t -> 
               case DVars.lookupDyad c (f,t) dv >>= DVars.send of
                   Just i  -> Map.insertWith' (+) i 1
                   Nothing -> id) si ts
    ri' = foldl' (flip $ \t ->
               case DVars.lookupDyad c (f,t) dv >>= DVars.receive of
                   Just i  -> Map.insertWith' (+) i 1
                   Nothing -> id) ri ts
    in Summary sv dv n' l' s' r' x' si' ri'
  where
    f = messageFrom m
    ts = messageTo m

module Actor (
    Actor(..),
    Sender,
    Receiver,
    
    ActorId,
    ReceiverId,
    SenderId,
    
    ActorSet,
    SenderSet,
    ReceiverSet,
    
    actorSet,
    actorIndex,
    actorAt,
    actorCount,
    actorAssocList,
    actorList,
  ) where

import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap

import Numeric.LinearAlgebra( Vector )

type ActorId = Int
type ReceiverId = Int
type SenderId = Int

data Actor =
    Actor { actorId :: !ActorId
          , actorVars :: !(Vector Double)
          }
    deriving (Show)
              
type Sender = Actor
type Receiver = Actor      

data ActorSet = ActorSet !Int !(IntMap Actor) !(IntMap Int)

type SenderSet = ActorSet
type ReceiverSet = ActorSet


actorSet :: [Actor] -> ActorSet
actorSet as = ActorSet (length as)
                       (IntMap.fromAscList $ zip [ 0.. ] as)
                       (IntMap.fromList $ zip (map actorId as) [ 0.. ])

actorIndex :: Actor -> ActorSet -> Maybe Int
actorIndex a (ActorSet _ _ idx) = IntMap.lookup (actorId a) idx

actorAt :: Int -> ActorSet -> Maybe Actor
actorAt i (ActorSet _ as _) = IntMap.lookup i as

actorCount :: ActorSet -> Int
actorCount (ActorSet n _ _) = n

actorAssocList :: ActorSet -> [(Int,Actor)]
actorAssocList (ActorSet _ as _) = IntMap.assocs as

actorList :: ActorSet -> [Actor]
actorList (ActorSet _ as _) = IntMap.elems as

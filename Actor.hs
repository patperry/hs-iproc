module Actor (
    Actor(..),
    Sender,
    Receiver,
    
    ActorId,
    ReceiverId,
    SenderId,
    
  ) where

import Numeric.LinearAlgebra( Vector )

type ActorId = Int
type ReceiverId = Int
type SenderId = Int

data Actor =
    Actor { actorVars :: !(Vector Double)
          }
    deriving (Eq, Show)
              
type Sender = Actor
type Receiver = Actor      

module Message 
    where

import Actor

type MessageId = Int
type Time = Int
type DiffTime = Int

data Message =
    Message { messageId :: !MessageId
            , messageTime :: !Time
            , messageFrom :: !ActorId
            , messageTo :: ![ActorId]
            }
    deriving (Show)

module Message (
    Message(..),
    MessageId,
    ) where

import Actor( SenderId, ReceiverId )

type MessageId = Int

data Message =
    Message { messageFrom :: !SenderId
            , messageTo :: ![ReceiverId]
            }
    deriving (Eq, Show)

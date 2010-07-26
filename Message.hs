module Message (
    Message(..)
    ) where

import Data.Time
import Actor

type MessageId = Int

data Message =
    Message { messageId :: !MessageId
            , messageTime :: !UTCTime
            , messageFrom :: !SenderId
            , messageTo :: ![ReceiverId]
            }
    deriving (Eq, Show)

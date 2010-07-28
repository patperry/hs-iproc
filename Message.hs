module Message (
    Message(..),
    MessageId,
    accumDVars,
    ) where

import Data.List( mapAccumL )        
import Data.Time

import Actor( SenderId, ReceiverId )
import DVars( DVars )
import qualified DVars as DVars

type MessageId = Int

data Message =
    Message { messageId :: !MessageId
            , messageTime :: !UTCTime
            , messageFrom :: !SenderId
            , messageTo :: ![ReceiverId]
            }
    deriving (Eq, Show)

accumDVars :: DVars -> [Message] -> (DVars, [(Message, DVars)])
accumDVars =
    mapAccumL (\d0 m -> 
            let d  = DVars.advanceTo (messageTime m) d0
                d' = DVars.insert (messageFrom m, messageTo m) d
            in (d',(m,d) ))

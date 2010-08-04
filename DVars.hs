module DVars (
    DVars,
    sendIntervals,
    receiveIntervals,
    fromIntervals,
    dim,
    index,
    
    Context,
    context,
    lookupDyad,
    lookupSender,
    
    DVar(..),
    send,
    receive,
    
    ) where
        
import qualified Data.Map as Map
import Data.Maybe( maybeToList )
import Data.Time

import Actor
import Context( Context )
import qualified Context as Context
import Intervals( Intervals, IntervalId )
import qualified Intervals as Intervals
import qualified History as History


data DVars =
    DVars { sendIntervals :: !Intervals
          , receiveIntervals :: !Intervals
          } deriving (Eq, Show)

data DVar = Send !IntervalId
          | Receive !IntervalId
    deriving (Eq, Show, Ord)

fromIntervals :: Intervals -> Intervals -> DVars
fromIntervals = DVars

dim :: DVars -> Int
dim (DVars si ri) = Intervals.size si + Intervals.size ri

index :: DVar -> DVars -> Int
index (Send i) _ = i
index (Receive i') (DVars si _) = Intervals.size si + i'

send :: DVar -> Maybe IntervalId
send dvar = case dvar of
    Send i -> Just i
    Receive _ -> Nothing

receive :: DVar -> Maybe IntervalId
receive dvar = case dvar of
    Send _ -> Nothing
    Receive i' -> Just i'
    
context :: UTCTime -> DVars -> Context
context t0 (DVars sint rint) = Context.empty sint rint t0

lookupSender :: Context -> SenderId -> DVars -> [(ReceiverId, [DVar])]
lookupSender c s _ =
    Map.toList $ Map.unionsWith (++) $ map Map.fromList
        [ [ (r, [Send i])
          | (r,i) <- History.pastEvents $ Context.senderHistory s c
          ]
        , [ (r, [Receive i'])
          | (r,i') <- History.pastEvents $ Context.receiverHistory s c
          ]
        ]
  where
    singleton a = [a]

lookupDyad :: Context -> SenderId -> ReceiverId -> DVars -> [DVar]
lookupDyad c s r _ =
    concatMap maybeToList
        [ fmap Send $ History.lookup r $ Context.senderHistory s c
        , fmap Receive $ History.lookup r $ Context.receiverHistory s c
        ]

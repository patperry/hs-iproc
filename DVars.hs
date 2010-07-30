module DVars (
    DVars,
    sendIntervals,
    receiveIntervals,
    fromIntervals,
    
    Context,
    context,
    lookupDyad,
    lookupSender,
    
    DVar(..),
    send,
    receive,
    sendReceive,
    
    ) where
        
import Control.Arrow( second )
import Data.List( foldl' )
import qualified Data.Map as Map
import Data.Time

import Actor
import Context( Context )
import qualified Context as Context
import Intervals( Intervals, IntervalId )
import qualified History as History


data DVars =
    DVars { sendIntervals :: !Intervals
          , receiveIntervals :: !Intervals
          } deriving (Eq, Show)

data DVar = Send !IntervalId
          | Receive !IntervalId
          | SendAndReceive !IntervalId !IntervalId
    deriving (Eq, Show)

fromIntervals :: Intervals -> Intervals -> DVars
fromIntervals = DVars

send :: DVar -> Maybe IntervalId
send dvar = case dvar of
    Send i -> Just i
    Receive _ -> Nothing
    SendAndReceive i _ -> Just i

receive :: DVar -> Maybe IntervalId
receive dvar = case dvar of
    Send _ -> Nothing
    Receive i' -> Just i'
    SendAndReceive _ i' -> Just i'
    
sendReceive :: DVar -> Maybe (IntervalId, IntervalId)
sendReceive dvar = case dvar of
    Send _ -> Nothing
    Receive _ -> Nothing
    SendAndReceive i i' -> Just (i,i')

context :: UTCTime -> DVars -> Context
context t0 (DVars sint rint) = Context.empty sint rint t0

lookupSender :: SenderId -> Context -> DVars -> [(ReceiverId, DVar)]
lookupSender s c _ = let
    m = Map.fromList $ map (second Send) $
            History.pastEvents $ Context.senderHistory s c
    m' = foldl' (flip $ uncurry $ Map.insertWith' (\(Receive j) (Send i) ->
                    SendAndReceive i j)) m $
            map (second Receive) $
                History.pastEvents $ Context.receiverHistory s c
    in Map.toList m'

lookupDyad :: (SenderId, ReceiverId) -> Context -> DVars -> Maybe DVar
lookupDyad (s,r) c _ = let
    mi = History.lookup r $ Context.senderHistory s c
    mj = History.lookup r $ Context.receiverHistory s c
    in case (mi,mj) of
        (Just i, Just j) -> Just $ SendAndReceive i j
        (Just i, Nothing) -> Just $ Send i
        (Nothing, Just j) -> Just $ Receive j
        (Nothing, Nothing) -> Nothing

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
import Data.Maybe( maybeToList, catMaybes )
import Data.Time

import Actor
import Context( Context )
import qualified Context as Context
import Intervals( Intervals, IntervalId )
import qualified Intervals as Intervals
import qualified EventSet as EventSet


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
context t0 (DVars sint rint) = Context.empty t0

lookupSender :: Context -> SenderId -> DVars -> [(ReceiverId, [DVar])]
lookupSender c s (DVars sint rint) =
    Map.toList $ Map.unionsWith (++) $ map (Map.fromList . catMaybes)
        [ [ case Intervals.lookup dt sint of
                Nothing -> Nothing
                Just i  -> Just (r, [Send i])
          | (r,dt) <- EventSet.past $ Context.lookupSender s c
          ]
        , [ case Intervals.lookup dt' rint of
                Nothing -> Nothing
                Just i' -> Just (r, [Receive i'])
          | (r,dt') <- EventSet.past $ Context.lookupReceiver s c
          ]
        ]

lookupDyad :: Context -> SenderId -> ReceiverId -> DVars -> [DVar]
lookupDyad c s r (DVars sint rint) =
    concatMap maybeToList
        [ do
              dt <- EventSet.lookup r $ Context.lookupSender s c
              i <- Intervals.lookup dt sint
              return $ Send i
        , do
              dt' <- EventSet.lookup r $ Context.lookupReceiver s c
              i' <- Intervals.lookup dt' rint
              return $ Receive i'
        ]

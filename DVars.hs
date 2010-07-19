module DVars (
    Intervals,
    IntervalId,
    DVars(..),
    dvarsWithIntervals,
    
    DVarsState,
    emptyDVarsState,
    updateDVarsState,
    logMessage,
    advanceDVarsStateTo,
    dvarsStateOf,
    
    ActorState,
    currentTime,
    sendHistory,
    recvHistory,

    History,
    recentEvents,
    lastEventOf,
    ) where
        
import Control.Monad.ST
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Data.Maybe( listToMaybe )
import Numeric.LinearAlgebra

import Actor
import Message

type Intervals = Vector DiffTime
type IntervalId = Int        
data DVars = 
    DVars { dvarsSendIntervals :: !Intervals
          , dvarsRecvIntervals :: !Intervals
          }
          
dvarsWithIntervals :: Intervals
                   -> Intervals
                   -> DVars
dvarsWithIntervals = DVars


data History = History !Intervals !(IntMap (DiffTime, IntervalId))

recentEvents :: History -> [(ActorId, IntervalId)]
recentEvents (History _ m) = [ (i, int) | (i,(_,int)) <- IntMap.assocs m ]

lastEventOf :: ActorId -> History -> Maybe IntervalId
lastEventOf a (History _ m) = do
    (_,int) <- IntMap.lookup a m
    return int

emptyHistory :: Vector DiffTime -> History
emptyHistory ints = History ints IntMap.empty

logEvent :: Int -> History -> History
logEvent i (History ints recent) =
    History ints (IntMap.insert i (0,0) recent)

advanceTimeBy :: DiffTime -> History -> History
advanceTimeBy delta h@(History ints recent)
    | delta == 0 = h
    | otherwise =
        let assocs = assocsVector ints
            recent' = flip IntMap.mapMaybe recent $ \(d,i) -> do
                          let d' = d + delta
                          i' <- listToMaybe [ i'
                                            | (i',int) <- drop i assocs
                                            , d' <= int
                                            ]
                          i' `seq` return (d',i')
        in History ints recent'                    

data ActorState =
    ActorState { currentTime :: !Time
               , sendHistory :: !History
               , recvHistory :: !History
               }

emptyActorState :: Time -> DVars -> ActorState
emptyActorState t0 (DVars sints rints) =
    ActorState t0 (emptyHistory sints) (emptyHistory rints)

advanceActorStateTo :: Time -> ActorState -> ActorState
advanceActorStateTo t' a@(ActorState t sh rh) | t' == t = a
                                              | otherwise =
    let delta = t' - t
        [ sh', rh' ] = map (advanceTimeBy delta) [ sh, rh ]
    in ActorState t' sh' rh'

logSend :: ActorId -> ActorState -> ActorState
logSend to (ActorState t sh rh) =
    let sh' = logEvent to sh
    in ActorState t sh' rh
    
logRecv :: ActorId -> ActorState -> ActorState
logRecv from (ActorState t sh rh) =
    let rh' = logEvent from rh
    in ActorState t sh rh'

                    
data DVarsState = DVarsState !Time !DVars !(IntMap ActorState)

emptyDVarsState :: Time -> DVars -> DVarsState
emptyDVarsState t0 dvars = DVarsState t0 dvars IntMap.empty

advanceDVarsStateTo :: Time -> DVarsState -> DVarsState
advanceDVarsStateTo t (DVarsState _ d am) = (DVarsState t d am)

dvarsStateOf :: ActorId -> DVarsState -> ActorState
dvarsStateOf a (DVarsState t dv am) =
    advanceActorStateTo t $ IntMap.findWithDefault empty a am
  where
    empty = emptyActorState t dv

logMessage :: (Int, [Int]) -> DVarsState -> DVarsState
logMessage (f,ts) (DVarsState time dvars am) =
    let def = emptyActorState time dvars
        a_f  = logSends ts $ IntMap.findWithDefault def f am
        a_ts = [ logRecv f $ IntMap.findWithDefault def t am | t <- ts ]
        am'  = foldr (\(k,v) -> v `seq` IntMap.insert k v) am $
                   (f,a_f):(zip ts a_ts)
    in DVarsState time dvars am'
  where
    logSends = flip (foldr logSend)

updateDVarsState :: Message -> DVarsState -> DVarsState
updateDVarsState (Message _ time f ts) =
    logMessage (f,ts) . advanceDVarsStateTo time

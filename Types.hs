{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module Types (
    Actor(..),
    ActorId,

    Sender,
    SenderId,

    Receiver,
    ReceiverId,
    

    Message(..),
    MessageId,
    
    
    Time,
    DiffTime,
    posixSecondsToTime,
    addTime,
    diffTime,
  ) where

import Numeric.LinearAlgebra( Vector )


#define FAST_TIME

#ifndef FAST_TIME

import Data.Time( NominalDiffTime, UTCTime, diffUTCTime, addUTCTime )
import Data.Time.Clock.POSIX( posixSecondsToUTCTime )

#else
    
type UTCTime = Int
type NominalDiffTime = Int

diffUTCTime, addUTCTime :: Int -> Int -> Int
diffUTCTime = (-)
addUTCTime = (+)

posixSecondsToUTCTime :: Int -> Int
posixSecondsToUTCTime = id

#endif


newtype Time = Time { unTime :: UTCTime }
    deriving (Eq, Show, Ord)
newtype DiffTime = DiffTime { unDiffTime :: NominalDiffTime } 
    deriving (Eq, Show, Num, Enum, Ord, Integral, Real)

posixSecondsToTime :: DiffTime -> Time
posixSecondsToTime = Time . posixSecondsToUTCTime . unDiffTime

diffTime :: Time -> Time -> DiffTime
diffTime t1 t2 = DiffTime $ diffUTCTime (unTime t1) (unTime t2)

addTime :: DiffTime -> Time -> Time
addTime dt t = Time $ addUTCTime (unDiffTime dt) (unTime t)



type ActorId = Int
type ReceiverId = Int
type SenderId = Int

data Actor =
    Actor { actorVars :: !(Vector Double)
          }
    deriving (Eq, Show)
              
type Sender = Actor
type Receiver = Actor      



type MessageId = Int

data Message =
    Message { messageFrom :: !SenderId
            , messageTo :: ![ReceiverId]
            }
    deriving (Eq, Show)


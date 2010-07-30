module Params (
    Params(..),
    defaultParams,
    validDyad,
    ) where
        

import Numeric.LinearAlgebra

import Actor( SenderId, ReceiverId )
import qualified Intervals as Intervals
import SVars( SVars )
import qualified SVars as SVars
import DVars( DVars )
import qualified DVars as DVars


data Params =
    Params { svars :: !SVars
           , dvars :: !DVars
           , staticCoefs :: !(Vector Double)
           , sendCoefs :: !(Vector Double)
           , receiveCoefs :: !(Vector Double)
           , hasSelfLoops :: !Bool
           }
    deriving (Eq, Show)

           
defaultParams :: SVars -> DVars -> Params
defaultParams sv dv =
    Params sv
           dv
           (constantVector (SVars.dim sv) 0)
           (constantVector (Intervals.size $ DVars.sendIntervals dv) 0)
           (constantVector (Intervals.size $ DVars.receiveIntervals dv) 0)
           False

validDyad :: (SenderId, ReceiverId) -> Params -> Bool
validDyad (s,r) p | hasSelfLoops p = True
                  | otherwise      = s /= r


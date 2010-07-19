module Params (
    Params(..),
    defaultParams,
    ) where
        
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor
import SVars
import DVars

data Params =
    Params { senderSet :: !ActorSet
           , receiverSet :: !ActorSet
           , staticCoefs :: !(Vector Double)
           , sendCoefs :: !(Vector Double)
           , recvCoefs :: !(Vector Double)
           , receiverEffects :: !(Map ReceiverId Double)
           , hasSelfLoops :: !Bool
           }
           
defaultParams :: ActorSet -> ActorSet SVars -> DVars -> Params
defaultParams ss rs sv dv = 
    Params ss
           rs
           (constantVector (svarsCount sv) 0)
           (constantVector (dimVector $ dvarsSendIntervals dv) 0)
           (constantVector (dimVector $ dvarsRecvIntervals dv) 0)
           Map.empty
           False

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
    Params { staticCoefs :: !(Vector Double)
           , sendCoefs :: !(Vector Double)
           , recvCoefs :: !(Vector Double)
           , receiverEffects :: !(Map ReceiverId Double)
           , hasSelfLoops :: !Bool
           }
           
defaultParams :: SVars -> DVars -> Params
defaultParams sv dv = 
    Params (constantVector (svarsCount sv) 0)
           (constantVector (dimVector $ dvarsSendIntervals dv) 0)
           (constantVector (dimVector $ dvarsRecvIntervals dv) 0)
           Map.empty
           False

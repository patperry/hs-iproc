module Param (
    Param(..),
    defaultParam,
    validDyad,
    ) where
        
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor
import SVars
import DVars

data Param =
    Param { senderSet :: !ActorSet
          , receiverSet :: !ActorSet
          , staticCoef :: !(Vector Double)
          , sendCoef :: !(Vector Double)
          , recvCoef :: !(Vector Double)
          , receiverEffect :: !(Map ReceiverId Double)
          , hasSelfLoops :: !Bool
          }
           
defaultParam :: ActorSet -> ActorSet -> SVars -> DVars -> Param
defaultParam ss rs sv dv = 
    Param ss
          rs
          (constantVector (svarsCount sv) 0)
          (constantVector (dimVector $ dvarsSendIntervals dv) 0)
          (constantVector (dimVector $ dvarsRecvIntervals dv) 0)
          Map.empty
          False

validDyad :: Param -> SenderId -> ReceiverId -> Bool
validDyad p s r | hasSelfLoops p = True
                | otherwise      = s /= r

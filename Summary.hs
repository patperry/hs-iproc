module Summary (
    Summary(..),
    emptySummary,
    updateSummary,
    listSummary,
    ) where

import Data.List( foldl' )       
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor
import Message
import SVars
import DVars

        
data Summary = 
    Summary { summarySVars :: SVars
            , summaryDVarsState :: DVarsState
            , messageCount :: !Int
            , messageLenCount :: !(Map Int Int)
            , sendCount :: !(Map SenderId Int)
            , recvCount :: !(Map ReceiverId Int)
            , staticVarsSum :: !(Vector Double)
            , sendIntervalsCount :: !(Map IntervalId Int)
            , recvIntervalsCount :: !(Map IntervalId Int)
            }

listSummary :: SVars -> DVars -> Time -> [Message] -> Summary
listSummary sv dv t0 =
    foldl' updateSummary (emptySummary sv $ emptyDVarsState t0 dv)
        
emptySummary :: SVars -> DVarsState -> Summary
emptySummary sv dv = 
    let x0 = constantVector (svarsCount sv) 0
    in Summary sv dv 0 Map.empty Map.empty Map.empty x0 Map.empty Map.empty
        
updateSummary :: Summary -> Message -> Summary
updateSummary (Summary sv dv n l s r x si ri) m@(Message _ time f ts) =
    let n' = n + 1
        l' = Map.insertWith' (+) (length ts) 1 l
        s' = Map.insertWith' (+) f 1 s
        r' = foldr (\t -> Map.insertWith' (+) t 1) r ts
        x' = foldr (\t -> addVector (svarsDyad f t sv)) x ts
        h  = dvarsStateOf f $ advanceDVarsStateTo time dv
        si' = foldr (\t -> case lastEventOf t (sendHistory h) of
                              Nothing -> id
                              Just int -> Map.insertWith' (+) int 1) si ts
        ri' = foldr (\t -> case lastEventOf t (recvHistory h) of
                              Nothing -> id
                              Just int -> Map.insertWith' (+) int 1) ri ts
        dv' = updateDVarsState m dv
    in Summary sv dv' n' l' s' r' x' si' ri'

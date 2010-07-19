module Summary (
    Summary(..),
    emptySummary,
    updateSummary,
    listSummary,
    ) where

import Data.List( foldl', mapAccumR )       
import Data.Map( Map )
import qualified Data.Map as Map

import Numeric.LinearAlgebra

import Actor
import Message
import SVars
import DVars

        
data Summary = 
    Summary { summarySVars :: SVars
            , summaryDVars :: DVars
            , messageCount :: !Int
            , messageLenCount :: !(Map Int Int)
            , sendCount :: !(Map SenderId Int)
            , recvCount :: !(Map ReceiverId Int)
            , staticVarsSum :: !(Vector Double)
            , sendIntervalsCount :: !(Map IntervalId Int)
            , recvIntervalsCount :: !(Map IntervalId Int)
            }

messageHistory :: DVarsState -> [Message] -> (DVarsState, [DVarsState])
messageHistory =
    mapAccumR (\h0 m -> 
        let h  = advanceDVarsStateTo (messageTime m) h0
            h' = logMessage (messageFrom m, messageTo m) h
        in (h',h))

listSummary :: SVars -> DVars -> Time -> [Message] -> Summary
listSummary sv dv t0 ms =
    let h0 = emptyDVarsState t0 dv
        hs = snd $ messageHistory h0 ms
    in foldl' updateSummary (emptySummary sv dv) $ zip ms hs
        
emptySummary :: SVars -> DVars -> Summary
emptySummary sv dv = 
    let x0 = constantVector (svarsCount sv) 0
    in Summary sv dv 0 Map.empty Map.empty Map.empty x0 Map.empty Map.empty
        
updateSummary :: Summary -> (Message, DVarsState) -> Summary
updateSummary (Summary sv dv n l s r x si ri) ((Message _ _ f ts), h) =
    let n' = n + 1
        l' = Map.insertWith' (+) (length ts) 1 l
        s' = Map.insertWith' (+) f 1 s
        r' = foldr (\t -> Map.insertWith' (+) t 1) r ts
        x' = foldr (\t -> addVector (svarsDyad f t sv)) x ts
        ah = dvarsStateOf f h
        si' = foldr (\t -> case lastEventOf t (sendHistory ah) of
                              Nothing -> id
                              Just int -> Map.insertWith' (+) int 1) si ts
        ri' = foldr (\t -> case lastEventOf t (recvHistory ah) of
                              Nothing -> id
                              Just int -> Map.insertWith' (+) int 1) ri ts
    in Summary sv dv n' l' s' r' x' si' ri'

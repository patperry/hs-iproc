module Summary (
    ) where
        
import Data.Map( Map )
import qualified Data.Map as Map

import Actor
import Message

        
data Summary = 
    Summary { messageCount :: !Int
            , messageLenCount :: !(Map Int Int)
            , sendCount :: !(Map SenderId Int)
            , recvCount :: !(Map ReceiverId Int)
            }
        
emptySummary :: Summary
emptySummary = Summary 0 Map.empty Map.empty Map.empty        
        
updateSummary :: Summary -> Message -> Summary
updateSummary (Summary n l s r) (Message _ _ f ts) =
    let n' = n + 1
        l' = Map.insertWith' (+) (length ts) 1 l
        s' = Map.insertWith' (+) f 1 s
        r' = foldr (\t -> Map.insertWith' (+) t 1) r ts
    in Summary n' l' s' r'

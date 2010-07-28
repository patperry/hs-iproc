module SVars (
    SVars( dim, senders, receivers ),

    lookupSender,
    lookupDyad,
    
    fromLists,
    -- fromListsWith,
    interactions,
  ) where
   
import Data.Function( on )
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Data.List( sortBy )
import Numeric.LinearAlgebra

import Actor


data SVars = 
    SVars { dim :: !Int
          , varMap :: !(IntMap [(ReceiverId, Vector Double)])
          , senders :: [Sender]
          , receivers :: [Receiver]
          }

interactions :: Sender -> Receiver -> Vector Double
interactions s r = (actorVars r) `kroneckerVector` (actorVars s)

fromLists :: [Sender] -> [Receiver] -> SVars
fromLists = fromListsWith interactions

fromListsWith :: (Sender -> Receiver -> Vector Double)
              -> [Sender]
              -> [Receiver]
              -> SVars
fromListsWith f ss rs | null ss = error "empty Sender list"
                      | null rs = error "empty Receiver list"
                      | otherwise = let
    p = dimVector $ f (head ss) (head rs)
    m = IntMap.fromList
            [ (actorId s, [ (actorId r, f s r) | r <- rs ])
            | s <- ss ]
    in SVars p m ss rs

lookupSender :: SenderId -> SVars -> [(ReceiverId, Vector Double)]
lookupSender s (SVars _ m _ _) =
    IntMap.findWithDefault (error "unknown sender") s m

lookupDyad :: (SenderId, ReceiverId) -> SVars -> Vector Double
lookupDyad (s,r) svars = let
    rs = lookupSender s svars
    in case lookup r rs of
           Nothing -> error "unknown receiver"
           Just x -> x
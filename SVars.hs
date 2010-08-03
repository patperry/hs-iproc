module SVars (
    SVars( dim, senders, receivers ),

    lookupSender,
    lookupDyad,
    
    fromActors,
    fromActorsWith,
    interactions,
  ) where
   
import Data.Map( Map )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import Actor


data SVars = 
    SVars { dim :: !Int
          , varMap :: !(Map SenderId (Map ReceiverId  (Vector Double)))
          , senders :: !(Map SenderId Sender)
          , receivers :: !(Map ReceiverId Receiver)
          } deriving (Eq, Show)

interactions :: Sender -> Receiver -> Vector Double
interactions s r = (actorVars r) `kroneckerVector` (actorVars s)

fromActors :: Map SenderId Sender -> Map ReceiverId Receiver -> SVars
fromActors = fromActorsWith interactions

fromActorsWith :: (Sender -> Receiver -> Vector Double)
               -> Map SenderId Sender
               -> Map ReceiverId Receiver
               -> SVars
fromActorsWith f ss rs | Map.null ss = error "empty Sender map"
                       | Map.null rs = error "empty Receiver map"
                       | otherwise = let
    p = dimVector $ f (snd $ Map.elemAt 0 ss) (snd $ Map.elemAt 0 rs)
    m = Map.fromList [
            (i, Map.fromList [ 
                    (j, f s r) | (j,r) <- Map.assocs rs ])
            | (i,s) <- Map.assocs ss ]
    in SVars p m ss rs

lookupSender :: SenderId -> SVars -> [(ReceiverId, Vector Double)]
lookupSender s (SVars _ m _ _) = Map.assocs $
    Map.findWithDefault (error "unknown sender") s m

lookupDyad :: SenderId -> ReceiverId -> SVars -> Vector Double
lookupDyad s r (SVars _ m _ _) = 
    Map.findWithDefault (error "unknown receiver") r $
         Map.findWithDefault (error "unknown sender") s m

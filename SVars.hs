module SVars (
    SVars,
    svarsCount,
    
    allDyadVars,
    svarsWith,
    svarsOutOf,
    svarsDyad,
  ) where
   
import Data.Function( on )
import Data.IntMap( IntMap )
import qualified Data.IntMap as IntMap
import Data.List( sortBy )
import Data.Maybe( fromJust )
import Numeric.LinearAlgebra

import Actor


data SVars = 
    SVars { svarsCount :: !Int
          , varMap :: !(IntMap [(ReceiverId, Vector Double)])
          }

allDyadVars :: Sender -> Receiver -> Vector Double
allDyadVars s r = (actorVars r) `kroneckerVector` (actorVars s)

svarsWith :: (Sender -> Receiver -> Vector Double)
          -> [Sender]
          -> [Receiver]
          -> SVars
svarsWith f ss rs =
    let p = dimVector $ f (head ss) (head rs)
        m = IntMap.fromList
                [ (actorId s, [ (actorId r, f s r) | r <- rs ])
                | s <- ss ]
    in SVars p m

svarsOutOf :: SenderId -> SVars -> [(ReceiverId, Vector Double)]
svarsOutOf s (SVars _ m) = IntMap.findWithDefault [] s m

svarsDyad :: SenderId -> ReceiverId -> SVars -> Vector Double
svarsDyad s r vars = fromJust $ lookup r (svarsOutOf s vars)

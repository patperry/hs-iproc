module IntervalSet (
    IntervalSet,
    IntervalId,
    
    size,
    at,
    lookup,
    assocs,
    
    fromList,
    toList,
    ) where
        
import Prelude hiding ( lookup )

import Data.List( findIndex, nub, sort )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( fromJust )
import Data.Time

type IntervalId = Int
data IntervalSet = IntervalSet !Int !(Map IntervalId DiffTime) deriving (Eq)

fromList :: [DiffTime] -> IntervalSet
fromList ts | sort ts /= ts = error "time interval list is not sorted"
            | nub ts /= ts = error "time interval list is not unique"
            | any (<= 0) ts = error "non-positive time interval"
            | otherwise =
        IntervalSet (length ts) $ Map.fromList $ zip [ 0.. ] ts

toList :: IntervalSet -> [DiffTime]
toList (IntervalSet _ m) = Map.elems m

instance Show IntervalSet where
    show iset = "fromList " ++ show (toList iset)


size :: IntervalSet -> Int
size (IntervalSet n _) = n

at :: IntervalId -> IntervalSet -> DiffTime
at i (IntervalSet n m) | i < 0 = error "negative index"
                       | i >= n = error "index too large"
                       | otherwise =
    fromJust $ Map.lookup i m

lookup ::  DiffTime -> IntervalSet -> Maybe IntervalId
lookup t (IntervalSet _ m) | t <= 0 = Nothing
                           | otherwise = do
    i <- findIndex (>= t) $ 0:(Map.elems m)
    return $ pred i
    
assocs :: IntervalSet -> [(IntervalId,DiffTime)]
assocs (IntervalSet _ m) = Map.assocs m


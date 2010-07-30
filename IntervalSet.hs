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
import Data.Time


type IntervalId = Int
newtype IntervalSet = IntervalSet [NominalDiffTime] deriving (Eq)

fromList :: [NominalDiffTime] -> IntervalSet
fromList ts | sort ts /= ts = error "time interval list is not sorted"
            | nub ts /= ts = error "time interval list is not unique"
            | any (<= 0) ts = error "non-positive time interval"
            | otherwise =
        IntervalSet ts

toList :: IntervalSet -> [NominalDiffTime]
toList (IntervalSet ts) = ts

instance Show IntervalSet where
    show iset = "fromList " ++ show (toList iset)


size :: IntervalSet -> Int
size (IntervalSet ts) = length ts

at :: IntervalId -> IntervalSet -> NominalDiffTime
at i (IntervalSet ts) = ts !! i

lookup ::  NominalDiffTime -> IntervalSet -> Maybe IntervalId
lookup t (IntervalSet ts) | t <= 0 = Nothing
                          | otherwise = do
    i <- findIndex (>= t) $ 0:ts
    return $ pred i
    
assocs :: IntervalSet -> [(IntervalId,NominalDiffTime)]
assocs (IntervalSet ts) = zip [ 0.. ] ts


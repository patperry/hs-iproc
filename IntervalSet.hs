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
data IntervalSet = IntervalSet !Int ![NominalDiffTime] deriving (Eq)

fromList :: [NominalDiffTime] -> IntervalSet
fromList ts | sort ts /= ts = error "time interval list is not sorted"
            | nub ts /= ts = error "time interval list is not unique"
            | any (<= 0) ts = error "non-positive time interval"
            | otherwise =
        IntervalSet (length ts) ts

toList :: IntervalSet -> [NominalDiffTime]
toList (IntervalSet _ ts) = ts

instance Show IntervalSet where
    show iset = "fromList " ++ show (toList iset)


size :: IntervalSet -> Int
size (IntervalSet n _) = n

at :: IntervalId -> IntervalSet -> NominalDiffTime
at i (IntervalSet _ ts) = ts !! i

lookup ::  NominalDiffTime -> IntervalSet -> Maybe IntervalId
lookup t (IntervalSet _ ts) | t <= 0 = Nothing
                            | otherwise = do
    i <- findIndex (>= t) $ 0:ts
    return $ pred i
    
assocs :: IntervalSet -> [(IntervalId,NominalDiffTime)]
assocs (IntervalSet _ ts) = zip [ 0.. ] ts


module Intervals (
    Intervals,
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
newtype Intervals = Intervals [NominalDiffTime] deriving (Eq)

fromList :: [NominalDiffTime] -> Intervals
fromList ts | sort ts /= ts = error "time interval list is not sorted"
            | nub ts /= ts = error "time interval list is not unique"
            | any (<= 0) ts = error "non-positive time interval"
            | otherwise =
        Intervals ts

toList :: Intervals -> [NominalDiffTime]
toList (Intervals ts) = ts

instance Show Intervals where
    show iset = "fromList " ++ show (toList iset)


size :: Intervals -> Int
size (Intervals ts) = length ts

at :: IntervalId -> Intervals -> NominalDiffTime
at i (Intervals ts) = ts !! i

lookup ::  NominalDiffTime -> Intervals -> Maybe IntervalId
lookup t (Intervals ts) | t <= 0 = Nothing
                          | otherwise = do
    i <- findIndex (>= t) $ 0:ts
    return $ pred i
    
assocs :: Intervals -> [(IntervalId,NominalDiffTime)]
assocs (Intervals ts) = zip [ 0.. ] ts


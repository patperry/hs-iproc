module Intervals (
    Intervals,
    
    size,
    at,
    lookup,
    assocs,
    
    empty,
    fromList,
    toList,
    ) where
        
import Prelude hiding ( lookup )

import Data.List( findIndex, nub, sort )
import Types( DiffTime )


data Intervals = Intervals !Int ![DiffTime] deriving (Eq)

empty :: Intervals
empty = fromList []

fromList :: [DiffTime] -> Intervals
fromList ts | sort ts /= ts = error "time interval list is not sorted"
            | nub ts /= ts = error "time interval list is not unique"
            | any (<= 0) ts = error "non-positive time interval"
            | otherwise =
        Intervals (length ts) ts

toList :: Intervals -> [DiffTime]
toList (Intervals _ ts) = ts

instance Show Intervals where
    show iset = "fromList " ++ show (toList iset)


size :: Intervals -> Int
size (Intervals n _) = n

at :: Int -> Intervals -> DiffTime
at i (Intervals _ ts) = ts !! i

lookup ::  DiffTime -> Intervals -> Maybe Int
lookup t (Intervals _ ts) | t <= 0 = Nothing
                          | otherwise = do
    i <- findIndex (>= t) $ 0:ts
    return $ pred i
    
assocs :: Intervals -> [(Int,DiffTime)]
assocs (Intervals _ ts) = zip [ 0.. ] ts


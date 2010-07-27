{-# LANGUAGE TupleSections #-}
module Properties
    where

import Data.Time
import Data.Time.Clock.POSIX
import Data.Function( on )
import Data.List( nub, nubBy, sort, sortBy )
import Data.Maybe( fromJust, mapMaybe )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( vector )
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.LinearAlgebra as Test
import Numeric.LinearAlgebra

import Actor( Actor(..) )

import ActorSet( ActorSet )
import qualified ActorSet as ActorSet

import History( History )
import qualified History as History

import IntervalSet(IntervalSet, IntervalId )
import qualified IntervalSet as IntervalSet

import SVars( SVars )
import qualified SVars as SVars

actor :: Int -> Gen Actor
actor p = do
    (NonNegative i) <- arbitrary
    actorWithId i p

actorWithId :: Int -> Int -> Gen Actor
actorWithId i p = do
    x <- Test.vector p
    return $ Actor i x

instance Arbitrary Actor where
    arbitrary = do
        p <-  choose (0,5)
        actor p
        
actorSet :: Int -> Gen ActorSet
actorSet p = do
    n <- choose (1, 20)
    actorSetWithSize n p

actorSetWithSize :: Int -> Int -> Gen ActorSet
actorSetWithSize n p = do
    is <- (mangle [] . sort) `fmap` QC.vector n
    as <- mapM (\i -> actorWithId i p) is
    return $ ActorSet.fromList as
  where
      mangle acc [] = reverse acc
      mangle acc (x:xs) = let
          x' = if x `notElem` acc then x else maximum acc + 1
          in mangle (x':acc) xs

instance Arbitrary ActorSet where
    arbitrary = do
        p <- choose (0,5)
        actorSet p

data ActorList = ActorList [Actor] deriving (Show)
instance Arbitrary ActorList where
    arbitrary = do
        as <- ActorSet.toList `fmap` arbitrary
        is <- QC.vector $ length as :: Gen [Int]
        let as' = (snd . unzip . sortBy (compare `on` fst)) $ zip is as
        return $ ActorList as'


instance Arbitrary DiffTime where
    arbitrary = do
        x <- arbitrary :: Gen Integer
        return $ fromIntegral x

data IntervalList = IntervalList [DiffTime] deriving (Show)
instance Arbitrary IntervalList where
    arbitrary = do
        ts <- arbitrary
        return $ IntervalList $ nub $ sort $ filter (> 0) ts

instance Arbitrary IntervalSet where
    arbitrary = do
        (IntervalList ts) <- arbitrary
        return $ IntervalSet.fromList ts

newtype EmptyIntervalSet = EmptyIntervalSet IntervalSet deriving (Eq,Show)
instance Arbitrary EmptyIntervalSet where
    arbitrary = return $ EmptyIntervalSet $ IntervalSet.fromList []

newtype NonEmptyIntervalSet = NonEmptyIntervalSet IntervalSet deriving (Eq,Show)
instance Arbitrary NonEmptyIntervalSet where
    arbitrary = do
        t <- arbitrary
        (IntervalList ts) <- arbitrary
        return $ NonEmptyIntervalSet $
            IntervalSet.fromList $ nub $ sort $ (abs t + 1):ts


instance Arbitrary UTCTime where
    arbitrary = do
        n <- arbitrary :: Gen Int
        return $ posixSecondsToUTCTime $ fromIntegral n

data EmptyHistory = EmptyHistory IntervalSet UTCTime
    deriving (Eq, Show)

instance Arbitrary EmptyHistory where
    arbitrary = do
        iset <- arbitrary
        t0 <- arbitrary
        return $ EmptyHistory iset t0

data UpdateHistory e = AdvanceBy DiffTime
                     | Insert e
    deriving (Eq, Show)
    
updateHistory :: (Ord e) => UpdateHistory e -> History e -> History e
updateHistory (AdvanceBy dt) = History.advanceBy dt
updateHistory (Insert e) = History.insert e
    
instance (Arbitrary e, Ord e) => Arbitrary (UpdateHistory e) where
    arbitrary = do
        dt <- fmap abs arbitrary
        e <- arbitrary
        elements [ AdvanceBy dt, Insert e]

instance (Arbitrary e, Ord e) => Arbitrary (History e) where
    arbitrary = do
        (EmptyHistory iset t0) <- arbitrary
        us <- arbitrary
        return $ foldr updateHistory (History.empty iset t0) us


tests_ActorSet = testGroup "ActorSet"
    [ testProperty "size . fromList" prop_ActorSet_size_fromList
    , testProperty "lookup . fromList" prop_ActorSet_lookup_fromList
    , testProperty "at . fromList" prop_ActorSet_at_fromList
    , testProperty "assocs . fromList" prop_ActorSet_assocs_fromList
    , testProperty "toList . fromList" prop_ActorSet_toList_fromList
    ]

prop_ActorSet_size_fromList (ActorList as) =
    (ActorSet.size . ActorSet.fromList) as == length as
    
prop_ActorSet_lookup_fromList (ActorList as) = let
    a_set = ActorSet.fromList as
    in and [ ActorSet.lookup (actorId a) a_set == Just i
           | (i,a) <- zip [ 0.. ] as 
           ]

prop_ActorSet_at_fromList (ActorList as) = let
    a_set = ActorSet.fromList as
    in and [ ActorSet.at i a_set == a
           | (i,a) <- zip [ 0.. ] as 
           ]
           
prop_ActorSet_assocs_fromList (ActorList as) =
    (ActorSet.assocs . ActorSet.fromList) as == zip [ 0.. ] as

prop_ActorSet_toList_fromList (ActorList as) =
    (ActorSet.toList . ActorSet.fromList) as == as


tests_SVars = testGroup "SVars"
    [ testProperty "interactions" prop_SVars_interactions
    , testProperty "dim . fromLists" prop_SVars_dim_fromLists
    , testProperty "senders . fromLists" prop_SVars_senders_fromLists
    , testProperty "receivers . fromLists" prop_SVars_receivers_fromLists
    , testProperty "lookupDyad . fromLists" prop_SVars_lookupDyad_fromLists
    , testProperty "lookupSender . fromLists" prop_SVars_lookupSender_fromLists
    ]

prop_SVars_interactions s r =
    SVars.interactions s r
        == listVector p [ xi * yj | yj <- elemsVector y
                                  , xi <- elemsVector x ]
  where
    x = actorVars s
    y = actorVars r
    p = dimVector x * dimVector y

prop_SVars_dim_fromLists (ActorList ss) (ActorList rs) =
    SVars.dim (SVars.fromLists ss rs)
        == dimVector (SVars.interactions s r)
  where
    s = head ss
    r = head rs

prop_SVars_senders_fromLists (ActorList ss) (ActorList rs) =
    SVars.senders (SVars.fromLists ss rs) == ss

prop_SVars_receivers_fromLists (ActorList ss) (ActorList rs) =
    SVars.receivers (SVars.fromLists ss rs) == rs

prop_SVars_lookupDyad_fromLists (ActorList ss) (ActorList rs) = let
    svars = SVars.fromLists ss rs
    in and [ SVars.lookupDyad (actorId s, actorId r) svars
                == Just (SVars.interactions s r)
           | s <- ss, r <- rs ]
    
prop_SVars_lookupSender_fromLists (ActorList ss) (ActorList rs) = let
    svars = SVars.fromLists ss rs
    in and [ SVars.lookupSender (actorId s) svars
                == Just [ (actorId r, SVars.interactions s r) | r <- rs ] 
           | s <- ss ]


tests_IntervalSet = testGroup "IntervalSet"
    [ testProperty "size . fromList" prop_IntervalSet_size_fromList
    , testProperty "at . fromList" prop_IntervalSet_at_fromList
    , testProperty "assocs . fromList" prop_IntervalSet_assocs_fromList     
    , testProperty "toList . fromList" prop_IntervalSet_toList_fromList 
    , testProperty "fromList . toList" prop_IntervalSet_fromList_toList
    , testProperty "lookup (empty)" prop_IntervalSet_lookup_empty
    , testProperty "lookup (nonpositive)" prop_IntervalSet_lookup_nonpos
    , testProperty "lookup (endpoint)" prop_IntervalSet_lookup_endpoint
    , testProperty "lookup (before endpoint)" prop_IntervalSet_lookup_before_endpoint    
    , testProperty "lookup (after endpoint)" prop_IntervalSet_lookup_after_endpoint        
    , testProperty "lookkup (beyond last)" prop_IntervalSet_lookup_beyond_last
    ]

prop_IntervalSet_size_fromList (IntervalList ts) =
    (IntervalSet.size . IntervalSet.fromList) ts == length ts
    
prop_IntervalSet_at_fromList (IntervalList ts) = let
    iset = IntervalSet.fromList ts
    in and [ IntervalSet.at i iset == t | (i,t) <- zip [ 0.. ] ts ]

prop_IntervalSet_assocs_fromList (IntervalList ts) =
    (IntervalSet.assocs . IntervalSet.fromList) ts == zip [ 0.. ] ts

prop_IntervalSet_toList_fromList (IntervalList ts) =
    (IntervalSet.toList . IntervalSet.fromList) ts == ts
    
prop_IntervalSet_fromList_toList iset =
    (IntervalSet.fromList . IntervalSet.toList) iset == iset
    
prop_IntervalSet_lookup_empty t (EmptyIntervalSet iset) =
    IntervalSet.lookup t iset == Nothing

prop_IntervalSet_lookup_nonpos t iset =
    IntervalSet.lookup (negate $ abs t) iset == Nothing
    
prop_IntervalSet_lookup_endpoint (NonEmptyIntervalSet iset) =
    forAll (choose (0,n-1)) $ \i -> let
        t = IntervalSet.at i iset
        in IntervalSet.lookup t iset == Just i
  where
    n = IntervalSet.size iset

prop_IntervalSet_lookup_before_endpoint (NonEmptyIntervalSet iset) =
    forAll (choose (0,n-1)) $ \i -> let
        t_begin = if i == 0 then 0 else IntervalSet.at (i-1) iset
        t_end = IntervalSet.at i iset
        t = t_end - picosecondsToDiffTime 1
        in IntervalSet.lookup t iset ==
            if t == 0 then Nothing
                      else if t == t_begin then Just (i-1)
                                           else Just i
  where
    n = IntervalSet.size iset

prop_IntervalSet_lookup_after_endpoint (NonEmptyIntervalSet iset) =
    forAll (choose (0,n-1)) $ \i -> let
        t_begin = if i == 0 then 0 else IntervalSet.at (i-1) iset
        t_end = IntervalSet.at i iset
        t = t_begin + picosecondsToDiffTime 1
        in IntervalSet.lookup t iset == Just i
  where
    n = IntervalSet.size iset

prop_IntervalSet_lookup_beyond_last (NonEmptyIntervalSet iset) =
    IntervalSet.lookup (tlast + picosecondsToDiffTime 1) iset == Nothing
  where
    n = IntervalSet.size iset
    tlast = IntervalSet.at (n - 1) iset


tests_History = testGroup "History"
    [ testProperty "pastEvents" prop_History_pastEvents
    , testProperty "currentEvents . insert" prop_History_currentEvents_insert
    , testProperty "pastEvents . advanceBy" prop_History_pastEvents_advanceBy
    , testProperty "lookup . advanceBy . insert" prop_History_lookup_advanceBy_insert
    ]
    
prop_History_pastEvents h =
    History.pastEvents h
        == map (\(e,dt) -> (e, fromJust $ IntervalSet.lookup dt iset))
               (History.pastEventsWithTimes h)
  where
    iset = History.intervalSet h
    _ = h :: History Int
    
prop_History_currentEvents_insert h e =
    (sort . History.currentEvents . History.insert e) h
    ==
    (sort . nub . (e:) . History.currentEvents) h
  where
    _ = h :: History Int
    
prop_History_pastEvents_advanceBy h (NonNegative dt) =
    sort ((History.pastEvents . History.advanceBy dt) h)
        == 
            (sort . nubBy ((==) `on` fst))
                 (mapMaybe (\e -> (e,) `fmap` IntervalSet.lookup dt iset)
                           (History.currentEvents h)
                  ++
                  mapMaybe (\(e,t) -> ((e,) `fmap` IntervalSet.lookup (t+dt) iset))
                           (History.pastEventsWithTimes h)
                 )
  where
    iset = History.intervalSet h
    _ = h :: History Int
    
prop_History_lookup_advanceBy_insert h e dt =
    (History.lookup e
     . History.advanceBy dt'
     . History.insert e) h
        == IntervalSet.lookup dt' (History.intervalSet h)
  where
    dt' = abs dt + picosecondsToDiffTime 1
    _ = e :: Int
        
    
main :: IO ()
main = defaultMain [ tests_ActorSet
                   , tests_SVars
                   , tests_IntervalSet
                   , tests_History
                   ]

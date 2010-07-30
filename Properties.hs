{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Main
    where

import Control.Monad( replicateM )
import Data.Time
import Data.Time.Clock.POSIX
import Data.Function( on )
import Data.List( foldl', foldl1', nub, nubBy, sort )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( fromJust, catMaybes, mapMaybe )
import qualified Data.Map as Map
import Debug.Trace
import System.Random( Random )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( vector )
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.LinearAlgebra as Test
import Numeric.LinearAlgebra

import Actor( Actor(..), ActorId, SenderId, ReceiverId )

import DVars( DVars, DVar(..) )
import qualified DVars as DVars

import History( History )
import qualified History as History

import IntervalSet(IntervalSet, IntervalId )
import qualified IntervalSet as IntervalSet

import Message

import Summary( Summary )
import qualified Summary as Summary

import SVars( SVars )
import qualified SVars as SVars

actor :: Int -> Gen Actor
actor p = do
    x <- Test.vector p
    return $ Actor x

instance Arbitrary Actor where
    arbitrary = do
        p <-  choose (0,5)
        actor p

actorMap :: Int -> Gen (Map ActorId Actor)
actorMap p = do
    i <- arbitrary
    is <- (i:) `fmap` arbitrary
    xs <- replicateM (length is) (actor p)
    return $ Map.fromList $ zip is xs
    
instance Arbitrary (Map ActorId Actor) where
    arbitrary = do
        p <- choose (0,5)
        actorMap p

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

data UpdateHistory e = HistoryAdvanceBy DiffTime
                     | HistoryInsert e
    deriving (Eq, Show)
    
updateHistory :: (Ord e) => UpdateHistory e -> History e -> History e
updateHistory (HistoryAdvanceBy dt) = History.advanceBy dt
updateHistory (HistoryInsert e) = History.insert e
    
instance (Arbitrary e, Ord e, Num e, Random e) => Arbitrary (UpdateHistory e) where
    arbitrary = do
        dt <- fmap abs arbitrary
        e <- choose (0,5)
        elements [ HistoryAdvanceBy dt, HistoryInsert e]

instance (Arbitrary e, Ord e, Num e, Random e) => Arbitrary (History e) where
    arbitrary = do
        (EmptyHistory iset t0) <- arbitrary
        us <- arbitrary
        return $ foldr updateHistory (History.empty iset t0) us

instance Arbitrary Message where
    arbitrary = message [0..5] [0..5]
        
svars :: [SenderId] -> [ReceiverId] -> Gen SVars
svars is js = do
        p <- choose (0,5)
        q <- choose (0,5)
        ss <- replicateM (length is) (actor p)
        rs <- replicateM (length js) (actor q)
        return $ SVars.fromActors (Map.fromList $ zip is ss)
                                  (Map.fromList $ zip js rs)

instance Arbitrary SVars where
    arbitrary = svars [ 0..5 ] [ 0..5 ]


dvars :: [SenderId] -> [ReceiverId]
      -> IntervalSet -> IntervalSet
      -> UTCTime
      -> Gen DVars
dvars is js sint rint t0 = do
    n <- choose (0,100)
    dts <- (sort . map (negate . abs)) `fmap` replicateM n arbitrary
    ms <- replicateM n $ message is js
    
    let ts = map ((`addUTCTime` t0) . realToFrac) (dts :: [DiffTime])
        dv0 = DVars.empty sint rint $ minimum (t0:ts)
        (dv,_) = DVars.accum dv0 $ zip ts ms
        dv' = DVars.advanceTo t0 dv
    
    return $ dv'
  
instance Arbitrary DVars where
    arbitrary = do
        (DVarsWithSender _ dv) <- arbitrary
        return dv

data DVarsWithSender = DVarsWithSender SenderId DVars deriving (Show)
instance Arbitrary DVarsWithSender where
    arbitrary = do
        sv <- arbitrary
        let ss = Map.keys $ SVars.senders sv
            rs = Map.keys $ SVars.receivers sv
        sint <- arbitrary
        rint <- arbitrary
        t0 <- arbitrary
        dv <- dvars ss rs sint rint t0  
        
        s0 <- arbitrary
        s <- elements (s0:ss)
        
        return $ DVarsWithSender s dv

data DVarsWithSameIntervalsAndSender =
        DVarsWithSameIntervalsAndSender SenderId DVars deriving (Show)
instance Arbitrary DVarsWithSameIntervalsAndSender where
    arbitrary = do
        sv <- arbitrary
        let ss = Map.keys $ SVars.senders sv
            rs = Map.keys $ SVars.receivers sv
        int <- arbitrary
        t0 <- arbitrary
        dv <- dvars ss rs int int t0  
        
        s0 <- arbitrary
        s <- elements (s0:ss)
        
        return $ DVarsWithSameIntervalsAndSender s dv


message :: [SenderId] -> [ReceiverId] -> Gen Message
message ss rs = do
    f <- elements ss
    l <- frequency [ (16, return 1)
                   , (8, return 2)
                   , (4, return 3)
                   , (2, return 4)
                   , (1, return 5)
                   ]
    ts <- fmap nub $ replicateM l $ elements rs
    return $ Message f ts

data MessagesWithVars =
    MessagesWithVars SVars DVars [(UTCTime, Message)] deriving (Show)
instance Arbitrary MessagesWithVars where
    arbitrary =
        let ss = [ 0..5 ]
            rs = [ 0..5 ]
        in do
            ms <- arbitrary
            ts <- sort `fmap` replicateM (length ms + 1) arbitrary
            sv <- svars ss rs
            sint <- arbitrary
            rint <- arbitrary
            dv <- dvars ss rs sint rint (minimum ts)
            return $ MessagesWithVars sv dv $ zip ts ms

data MessageWithVars =
    MessageWithVars SVars DVars Message deriving (Show)
instance Arbitrary MessageWithVars where
    arbitrary = do
        (MessagesWithVars sv dv _) <- arbitrary
        let ss = Map.keys $ SVars.senders sv
            rs = Map.keys $ SVars.receivers sv
        m <- message ss rs
        return $ MessageWithVars sv dv m

        

tests_SVars = testGroup "SVars"
    [ testProperty "interactions" prop_SVars_interactions
    , testProperty "dim . fromActors" prop_SVars_dim_fromActors
    , testProperty "senders . fromActors" prop_SVars_senders_fromActors
    , testProperty "receivers . fromActors" prop_SVars_receivers_fromActors
    , testProperty "lookupDyad . fromActors" prop_SVars_lookupDyad_fromActors
    , testProperty "lookupSender . fromActors" prop_SVars_lookupSender_fromActors
    ]

prop_SVars_interactions s r =
    SVars.interactions s r
        == listVector p [ xi * yj | yj <- elemsVector y
                                  , xi <- elemsVector x ]
  where
    x = actorVars s
    y = actorVars r
    p = dimVector x * dimVector y

prop_SVars_dim_fromActors ss rs =
    SVars.dim (SVars.fromActors ss rs)
        == dimVector (SVars.interactions s r)
  where
    s = snd $ Map.elemAt 0 ss
    r = snd $ Map.elemAt 0 rs

prop_SVars_senders_fromActors ss rs =
    SVars.senders (SVars.fromActors ss rs) == ss

prop_SVars_receivers_fromActors ss rs =
    SVars.receivers (SVars.fromActors ss rs) == rs

prop_SVars_lookupDyad_fromActors ss rs = let
    sv = SVars.fromActors ss rs
    in and [ SVars.lookupDyad (i,j) sv
                == SVars.interactions s r
           | (i,s) <- Map.assocs ss, (j,r) <- Map.assocs rs ]
    
prop_SVars_lookupSender_fromActors ss rs = let
    sv = SVars.fromActors ss rs
    in and [ SVars.lookupSender i sv
                == [ (j, SVars.interactions s r) | (j,r) <- Map.assocs rs ] 
           | (i,s) <- Map.assocs ss ]


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


tests_DVars = testGroup "DVars"
        [ testProperty "senderHistory" prop_DVars_senderHistory
        , testProperty "receiverHistory" prop_DVars_receiverHistory
        , testProperty "lookupSender" prop_DVars_lookupSender
        , testProperty "lookupDyad (dual)" prop_DVars_lookupDyad_dual
        ]

prop_DVars_senderHistory dv (Message f ts) (NonNegative dt) =
        (History.advanceBy dt
         . flip (foldr History.insert) ts
         . DVars.senderHistory f) dv
        ==
        (DVars.senderHistory f
         . DVars.advanceBy dt
         . DVars.insert (Message f ts)) dv

prop_DVars_receiverHistory dv (Message f ts) (NonNegative dt) =
    flip all ts $ \t ->
        (History.advanceBy dt
         . History.insert f
         . DVars.receiverHistory t) dv
        ==
        (DVars.receiverHistory t
         . DVars.advanceBy dt
         . DVars.insert (Message f ts)) dv

prop_DVars_lookupSender (DVarsWithSender s dv) = let
    rds = DVars.lookupSender s dv
    in rds == [ (r, fromJust $ DVars.lookupDyad (s,r) dv) | (r,_) <- rds ]

prop_DVars_lookupDyad_dual (DVarsWithSameIntervalsAndSender s dv) = let
    rds = DVars.lookupSender s dv
    in and [ fmap dual (DVars.lookupDyad (s,r) dv)
                 == DVars.lookupDyad (r,s) dv
           | (r,_) <- rds
           ]
  where
    dual (Send k) = Receive k
    dual (Receive l) = Send l
    dual (SendAndReceive k l) = SendAndReceive l k


tests_Summary = testGroup "Summary"
        [ testProperty "singleton" prop_Summary_singleton
        , testProperty "fromList" prop_Summary_fromList
        ]

prop_Summary_singleton (MessageWithVars s d m) = and
    [ Summary.messageCount smry == 1
    , Summary.messageLengthCount smry == Map.singleton (length ts) 1
    , Summary.sendCount smry == Map.singleton f (length ts)
    , Summary.receiveCount smry == Map.fromList (zip ts (repeat 1))
    , Summary.svarsSum smry
        == foldl1' addVector [ SVars.lookupDyad (f,t) s | t <- ts ]
    , Summary.dvarsSendSum smry
        == foldl' (flip $ \i -> Map.insertWith' (+) i 1)
                  Map.empty
                  (catMaybes [ DVars.lookupDyad (f,t) d
                               >>= DVars.sendIntervalId
                             | t <- ts ])
    , Summary.dvarsReceiveSum smry
        == foldl' (flip $ \i -> Map.insertWith' (+) i 1)
                  Map.empty
                  (catMaybes [ DVars.lookupDyad (f,t) d
                               >>= DVars.receiveIntervalId
                             | t <- ts ])
    ]
  where
    f = messageFrom m
    ts = messageTo m
    smry = Summary.singleton s (d,m)

prop_Summary_fromList (MessagesWithVars s d tms) =
    Summary.fromList s dms
        == foldl' Summary.union
                  (Summary.empty s)
                  (map (Summary.singleton s) dms)
  where
    (_,dms) = DVars.accum d tms

    
main :: IO ()
main = defaultMain [ tests_SVars
                   , tests_IntervalSet
                   , tests_History
                   , tests_DVars
                   , tests_Summary
                   ]

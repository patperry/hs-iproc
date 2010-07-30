{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Main
    where

import Control.Monad( replicateM )
import Data.Time
import Data.Time.Clock.POSIX( posixSecondsToUTCTime )
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

import Context( Context )
import qualified Context as Context

import DVars( DVars, DVar(..) )
import qualified DVars as DVars

import History( History )
import qualified History as History

import Intervals(Intervals, IntervalId )
import qualified Intervals as Intervals

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

data ActorIdList = ActorIdList [ActorId] deriving (Eq, Show)
instance Arbitrary ActorIdList where
    arbitrary = do
        l <- choose (1,5)
        (ActorIdList . nub) `fmap` replicateM l arbitrary
    
instance Arbitrary (Map ActorId Actor) where
    arbitrary = do
        p <- choose (0,5)
        actorMap p

instance Arbitrary NominalDiffTime where
    arbitrary = do
        x <- arbitrary :: Gen Integer
        return $ fromIntegral x

data IntervalList = IntervalList [NominalDiffTime] deriving (Show)
instance Arbitrary IntervalList where
    arbitrary = do
        ts <- arbitrary
        return $ IntervalList $ nub $ sort $ filter (> 0) ts

instance Arbitrary Intervals where
    arbitrary = do
        (IntervalList ts) <- arbitrary
        return $ Intervals.fromList ts

newtype EmptyIntervals = EmptyIntervals Intervals deriving (Eq,Show)
instance Arbitrary EmptyIntervals where
    arbitrary = return $ EmptyIntervals $ Intervals.fromList []

newtype NonEmptyIntervals = NonEmptyIntervals Intervals deriving (Eq,Show)
instance Arbitrary NonEmptyIntervals where
    arbitrary = do
        t <- arbitrary
        (IntervalList ts) <- arbitrary
        return $ NonEmptyIntervals $
            Intervals.fromList $ nub $ sort $ (abs t + 1):ts

instance Arbitrary UTCTime where
    arbitrary = do
        n <- arbitrary :: Gen Int
        return $ posixSecondsToUTCTime $ fromIntegral n

data EmptyHistory = EmptyHistory Intervals UTCTime
    deriving (Eq, Show)

instance Arbitrary EmptyHistory where
    arbitrary = do
        iset <- arbitrary
        t0 <- arbitrary
        return $ EmptyHistory iset t0

data UpdateHistory e = HistoryAdvanceBy NominalDiffTime
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
    arbitrary = do
        (ActorIdList ss) <- arbitrary
        (ActorIdList rs) <- arbitrary
        message ss rs

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

svars :: [SenderId] -> [ReceiverId] -> Gen SVars
svars is js = do
        p <- choose (0,5)
        q <- choose (0,5)
        ss <- replicateM (length is) (actor p)
        rs <- replicateM (length js) (actor q)
        return $ SVars.fromActors (Map.fromList $ zip is ss)
                                  (Map.fromList $ zip js rs)

instance Arbitrary SVars where
    arbitrary = do
        (ActorIdList ss) <- arbitrary
        (ActorIdList rs) <- arbitrary
        svars ss rs

instance Arbitrary DVars where
    arbitrary = do
        sint <- arbitrary
        rint <- arbitrary
        return $ DVars.fromIntervals sint rint
        
context :: [SenderId] -> [ReceiverId]
        -> UTCTime
        -> DVars
        -> Gen Context
context is js t0 dv = do
    n <- choose (0,100)
    dts <- (sort . map (negate . abs)) `fmap` replicateM n arbitrary
    ms <- replicateM n $ message is js
    
    let ts = map (`addUTCTime` t0) dts
        c0 = DVars.context (minimum (t0:ts)) dv
        c = fst $ Context.accum c0 $ zip ts ms
        c' = Context.advanceTo t0 c
    
    return $ c'

data DVarsWithContext = DVarsWithContext Context DVars deriving (Show)
instance Arbitrary DVarsWithContext where
    arbitrary = do
        dv <- arbitrary
        (ActorIdList is) <- arbitrary
        (ActorIdList js) <- arbitrary
        t0 <- arbitrary
        c <- context is js t0 dv
        return $ DVarsWithContext c dv

data MessageWithContext = MessageWithContext Context Message deriving (Show)
instance Arbitrary MessageWithContext where
    arbitrary = do
        dv <- arbitrary
        (ActorIdList is) <- arbitrary
        (ActorIdList js) <- arbitrary
        t0 <- arbitrary
        c <- context is js t0 dv
        m <- message is js
        return $ MessageWithContext c m
    
data MessagesWithVars =
    MessagesWithVars SVars DVars Context [(UTCTime, Message)] deriving (Show)
instance Arbitrary MessagesWithVars where
    arbitrary = do
        (ActorIdList ss) <- arbitrary
        (ActorIdList rs) <- arbitrary

        t <- arbitrary
        (t0:ts) <- fmap (sort . (t:)) arbitrary
        ms <- replicateM (length ts) $ message ss rs

        sv <- svars ss rs
        dv <- arbitrary
        c <- context ss rs t0 dv
        
        return $ MessagesWithVars sv dv c $ zip ts ms

data MessageWithVars =
    MessageWithVars SVars DVars Context Message deriving (Show)
instance Arbitrary MessageWithVars where
    arbitrary = do
        (MessagesWithVars sv dv c _) <- arbitrary
        let ss = Map.keys $ SVars.senders sv
            rs = Map.keys $ SVars.receivers sv
        m <- message ss rs
        return $ MessageWithVars sv dv c m

    
    
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


tests_Intervals = testGroup "Intervals"
    [ testProperty "size . fromList" prop_Intervals_size_fromList
    , testProperty "at . fromList" prop_Intervals_at_fromList
    , testProperty "assocs . fromList" prop_Intervals_assocs_fromList     
    , testProperty "toList . fromList" prop_Intervals_toList_fromList 
    , testProperty "fromList . toList" prop_Intervals_fromList_toList
    , testProperty "lookup (empty)" prop_Intervals_lookup_empty
    , testProperty "lookup (nonpositive)" prop_Intervals_lookup_nonpos
    , testProperty "lookup (endpoint)" prop_Intervals_lookup_endpoint
    , testProperty "lookup (before endpoint)" prop_Intervals_lookup_before_endpoint    
    , testProperty "lookup (after endpoint)" prop_Intervals_lookup_after_endpoint        
    , testProperty "lookkup (beyond last)" prop_Intervals_lookup_beyond_last
    ]

prop_Intervals_size_fromList (IntervalList ts) =
    (Intervals.size . Intervals.fromList) ts == length ts
    
prop_Intervals_at_fromList (IntervalList ts) = let
    iset = Intervals.fromList ts
    in and [ Intervals.at i iset == t | (i,t) <- zip [ 0.. ] ts ]

prop_Intervals_assocs_fromList (IntervalList ts) =
    (Intervals.assocs . Intervals.fromList) ts == zip [ 0.. ] ts

prop_Intervals_toList_fromList (IntervalList ts) =
    (Intervals.toList . Intervals.fromList) ts == ts
    
prop_Intervals_fromList_toList iset =
    (Intervals.fromList . Intervals.toList) iset == iset
    
prop_Intervals_lookup_empty t (EmptyIntervals iset) =
    Intervals.lookup t iset == Nothing

prop_Intervals_lookup_nonpos t iset =
    Intervals.lookup (negate $ abs t) iset == Nothing
    
prop_Intervals_lookup_endpoint (NonEmptyIntervals iset) =
    forAll (choose (0,n-1)) $ \i -> let
        t = Intervals.at i iset
        in Intervals.lookup t iset == Just i
  where
    n = Intervals.size iset

prop_Intervals_lookup_before_endpoint (NonEmptyIntervals iset) =
    forAll (choose (0,n-1)) $ \i -> let
        t_begin = if i == 0 then 0 else Intervals.at (i-1) iset
        t_end = Intervals.at i iset
        t = pred t_end
        in Intervals.lookup t iset ==
            if t == 0 then Nothing
                      else if t == t_begin then Just (i-1)
                                           else Just i
  where
    n = Intervals.size iset

prop_Intervals_lookup_after_endpoint (NonEmptyIntervals iset) =
    forAll (choose (0,n-1)) $ \i -> let
        t_begin = if i == 0 then 0 else Intervals.at (i-1) iset
        t = succ t_begin
        in Intervals.lookup t iset == Just i
  where
    n = Intervals.size iset

prop_Intervals_lookup_beyond_last (NonEmptyIntervals iset) =
    Intervals.lookup (succ tlast) iset == Nothing
  where
    n = Intervals.size iset
    tlast = Intervals.at (n - 1) iset


tests_History = testGroup "History"
    [ testProperty "pastEvents" prop_History_pastEvents
    , testProperty "currentEvents . insert" prop_History_currentEvents_insert
    , testProperty "pastEvents . advanceBy" prop_History_pastEvents_advanceBy
    , testProperty "lookup . advanceBy . insert" prop_History_lookup_advanceBy_insert
    ]
    
prop_History_pastEvents h =
    History.pastEvents h
        == map (\(e,dt) -> (e, fromJust $ Intervals.lookup dt is))
               (History.pastEventsWithTimes h)
  where
    is = History.intervals h
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
                 (mapMaybe (\e -> (e,) `fmap` Intervals.lookup dt is)
                           (History.currentEvents h)
                  ++
                  mapMaybe (\(e,t) -> ((e,) `fmap` Intervals.lookup (t+dt) is))
                           (History.pastEventsWithTimes h)
                 )
  where
    is = History.intervals h
    _ = h :: History Int
    
prop_History_lookup_advanceBy_insert h e (NonNegative dt) =
    (History.lookup e
     . History.advanceBy dt'
     . History.insert e) h
        == Intervals.lookup dt' (History.intervals h)
  where
    dt' = succ dt
    _ = e :: Int

tests_Context = testGroup "Context"
        [ testProperty "senderHistory" prop_Context_senderHistory
        , testProperty "receiverHistory" prop_Context_receiverHistory
        ]

prop_Context_senderHistory (MessageWithContext c m) (NonNegative dt) =
        (History.advanceBy dt
         . flip (foldr History.insert) ts
         . Context.senderHistory f) c
        ==
        (Context.senderHistory f
         . Context.advanceBy dt
         . Context.insert m) c
  where
    f = messageFrom m
    ts = messageTo m

prop_Context_receiverHistory (MessageWithContext c m) (NonNegative dt) =
    flip all ts $ \t ->
        (History.advanceBy dt
         . History.insert f
         . Context.receiverHistory t) c
        ==
        (Context.receiverHistory t
         . Context.advanceBy dt
         . Context.insert m) c
  where
    f = messageFrom m
    ts = messageTo m

tests_DVars = testGroup "DVars"
        [ testProperty "lookupSender" prop_DVars_lookupSender
        , testProperty "lookupDyad (dual)" prop_DVars_lookupDyad_dual
        ]

prop_DVars_lookupSender (DVarsWithContext c dv) =
    flip all (Context.senders c) $ \s -> let
        rds = DVars.lookupSender s c dv
        in rds == [ (r, fromJust $ DVars.lookupDyad (s,r) c dv)
                  | (r,_) <- rds ]

prop_DVars_lookupDyad_dual (ActorIdList ss) (ActorIdList rs) t0 int =
    forAll (context ss rs t0 dv) $ \c ->
        flip all (Context.senders c) $ \s ->
        flip all (Context.receivers c) $ \r ->
            fmap dual (DVars.lookupDyad (s,r) c dv)
                == DVars.lookupDyad (r,s) c dv
  where
    dv = DVars.fromIntervals int int

    dual (Send k) = Receive k
    dual (Receive l) = Send l
    dual (SendAndReceive k l) = SendAndReceive l k


tests_Summary = testGroup "Summary"
        [ testProperty "singleton" prop_Summary_singleton
        , testProperty "fromList" prop_Summary_fromList
        ]

prop_Summary_singleton (MessageWithVars s d c m) = and
    [ Summary.messageCount smry == 1
    , Summary.messageLengthCount smry == Map.singleton (length ts) 1
    , Summary.sendCount smry == Map.singleton f (length ts)
    , Summary.receiveCount smry == Map.fromList (zip ts (repeat 1))
    , Summary.svarsSum smry
        == foldl1' addVector [ SVars.lookupDyad (f,t) s | t <- ts ]
    , Summary.dvarsSendSum smry
        == foldl' (flip $ \i -> Map.insertWith' (+) i 1)
                  Map.empty
                  (catMaybes [ DVars.lookupDyad (f,t) c d
                               >>= DVars.send
                             | t <- ts ])
    , Summary.dvarsReceiveSum smry
        == foldl' (flip $ \i -> Map.insertWith' (+) i 1)
                  Map.empty
                  (catMaybes [ DVars.lookupDyad (f,t) c d
                               >>= DVars.receive
                             | t <- ts ])
    ]
  where
    f = messageFrom m
    ts = messageTo m
    smry = Summary.singleton s d (c,m)

prop_Summary_fromList (MessagesWithVars s d c tms) =
    Summary.fromList s d cms
        == foldl' Summary.union
                  (Summary.empty s d)
                  (map (Summary.singleton s d) cms)
  where
    (_,cms) = Context.accum c tms

    
main :: IO ()
main = defaultMain [ tests_SVars
                   , tests_Intervals
                   , tests_History
                   , tests_Context
                   , tests_DVars
                   , tests_Summary
                   ]

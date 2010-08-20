{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Main
    where

import Control.Arrow( second )
import Control.Monad( replicateM )
import Data.AEq( AEq(..) )
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

import History( History )
import qualified History as History

import DVars( DVars, DVar(..) )
import qualified DVars as DVars

import EventSet( EventSet )
import qualified EventSet as EventSet

import Intervals(Intervals, IntervalId )
import qualified Intervals as Intervals

import Message

import Model( SenderModel, ReceiverModel )
import qualified Model as Model

import Params( Params )
import qualified Params as Params

import Summary( Summary )
import qualified Summary as Summary

import SVars( SVars )
import qualified SVars as SVars

instance AEq DVar where
    (~==) = (==)
    (===) = (==)

actor :: Int -> Gen Actor
actor p = do
    x <- listVector p `fmap` replicateM p (choose (-1,1))
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

data UpdateEventSet e = EventSetAdvanceBy NominalDiffTime
                     | EventSetInsert e
    deriving (Eq, Show)
    
updateEventSet :: (Ord e) => UpdateEventSet e -> EventSet e -> EventSet e
updateEventSet (EventSetAdvanceBy dt) = EventSet.advanceBy dt
updateEventSet (EventSetInsert e) = EventSet.insert e
    
instance (Arbitrary e, Ord e, Num e, Random e) => Arbitrary (UpdateEventSet e) where
    arbitrary = do
        dt <- fmap abs arbitrary
        e <- choose (0,5)
        elements [ EventSetAdvanceBy dt, EventSetInsert e]

instance (Arbitrary e, Ord e, Num e, Random e) => Arbitrary (EventSet e) where
    arbitrary = do
        t0 <- arbitrary
        us <- arbitrary
        return $ foldr updateEventSet (EventSet.empty t0) us

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
        
history :: [SenderId] -> [ReceiverId]
        -> UTCTime
        -> DVars
        -> Gen History
history is js t0 dv = do
    n <- choose (0,100)
    dts <- (sort . map (negate . abs)) `fmap` replicateM n arbitrary
    ms <- replicateM n $ message is js
    
    let ts = map (`addUTCTime` t0) dts
        c0 = DVars.context (minimum (t0:ts)) dv
        c = fst $ History.accum c0 $ zip ts ms
        c' = History.advanceTo t0 c
    
    return $ c'

data DVarsWithHistory = DVarsWithHistory History DVars deriving (Show)
instance Arbitrary DVarsWithHistory where
    arbitrary = do
        dv <- arbitrary
        (ActorIdList is) <- arbitrary
        (ActorIdList js) <- arbitrary
        t0 <- arbitrary
        h <- history is js t0 dv
        return $ DVarsWithHistory h dv

data MessageWithHistory = MessageWithHistory History Message deriving (Show)
instance Arbitrary MessageWithHistory where
    arbitrary = do
        dv <- arbitrary
        (ActorIdList is) <- arbitrary
        (ActorIdList js) <- arbitrary
        t0 <- arbitrary
        h <- history is js t0 dv
        m <- message is js
        return $ MessageWithHistory h m
    
data MessagesWithVars =
    MessagesWithVars SVars DVars History [(UTCTime, Message)] deriving (Show)
instance Arbitrary MessagesWithVars where
    arbitrary = do
        (ActorIdList ss) <- arbitrary
        (ActorIdList rs) <- arbitrary

        t <- arbitrary
        (t0:ts) <- fmap (sort . (t:)) arbitrary
        ms <- replicateM (length ts) $ message ss rs

        sv <- svars ss rs
        dv <- arbitrary
        h <- history ss rs t0 dv
        
        return $ MessagesWithVars sv dv h $ zip ts ms

data MessageWithVars =
    MessageWithVars SVars DVars History Message deriving (Show)
instance Arbitrary MessageWithVars where
    arbitrary = do
        (MessagesWithVars sv dv h _) <- arbitrary
        let ss = Map.keys $ SVars.senders sv
            rs = Map.keys $ SVars.receivers sv
        m <- message ss rs
        return $ MessageWithVars sv dv h m


params :: SVars -> DVars -> Gen Params
params sv dv = let
    ps = SVars.dim sv
    pd = DVars.dim dv
    in do
        sc <- listVector ps `fmap` replicateM ps (choose (-5,5))
        dc <- listVector pd `fmap` replicateM pd (choose (-5,5))
        l <- arbitrary
        return $ (Params.defaultParams sv dv)
                     { Params.scoefs = sc
                     , Params.dcoefs = dc
                     , Params.hasSelfLoops = l
                     }

data ParamsWithVars =
    ParamsWithVars SVars DVars History Params deriving (Show)
instance Arbitrary ParamsWithVars where
    arbitrary = do
        (MessageWithVars sv dv h _) <- arbitrary
        p <- params sv dv
        return $ ParamsWithVars sv dv h p

data SenderModelWithHistory =
    SenderModelWithHistory History SenderModel deriving (Show)
instance Arbitrary SenderModelWithHistory where
    arbitrary = do
        (ActorIdList ss) <- arbitrary
        s <- elements ss
        (ActorIdList rs0) <- arbitrary -- make sure there is a non-loop
        let rs = nub $ (s+1):rs0
        
        t0 <- arbitrary
        sv <- svars ss rs
        dv <- arbitrary
        h <- history ss rs t0 dv
        p <- params sv dv
        return $ SenderModelWithHistory h $ Model.senderModel p s



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
    in and [ SVars.lookupDyad i j sv
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


tests_EventSet = testGroup "EventSet"
    [ testProperty "current . insert" prop_EventSet_current_insert
    , testProperty "past . advanceBy" prop_EventSet_past_advanceBy
    , testProperty "lookup . advanceBy . insert" prop_EventSet_lookup_advanceBy_insert
    ]
    
prop_EventSet_current_insert h e =
    (sort . EventSet.current . EventSet.insert e) h
    ==
    (sort . nub . (e:) . EventSet.current) h
  where
    _ = h :: EventSet Int
    
prop_EventSet_past_advanceBy h (NonNegative dt)
    | dt == 0 =
        ((EventSet.past . EventSet.advanceBy dt) h)
            `eq`
            (EventSet.past h)
    | otherwise =
        ((EventSet.past . EventSet.advanceBy dt) h)
            `eq`
            (nubBy ((==) `on` fst))
                 (map (\e -> (e,dt)) (EventSet.current h)
                  ++
                  map (\(e,t) -> (e,t+dt)) (EventSet.past h)
                 )
  where
    eq xs ys = sort xs == sort ys
    _ = h :: EventSet Int
    
prop_EventSet_lookup_advanceBy_insert h e (NonNegative dt) =
    (EventSet.lookup e
     . EventSet.advanceBy dt'
     . EventSet.insert e) h
        == Just dt'
  where
    dt' = succ dt
    _ = e :: Int

tests_History = testGroup "History"
    [ testProperty "lookupSender" prop_History_lookupSender
    , testProperty "lookupReceiver" prop_History_lookupReceiver
    ]

prop_History_lookupSender (MessageWithHistory c m) (NonNegative dt) =
        (EventSet.advanceBy dt
         . flip (foldr EventSet.insert) ts
         . History.lookupSender f) c
        ==
        (History.lookupSender f
         . History.advanceBy dt
         . History.insert m) c
  where
    f = messageFrom m
    ts = messageTo m

prop_History_lookupReceiver (MessageWithHistory c m) (NonNegative dt) =
    flip all ts $ \t ->
        (EventSet.advanceBy dt
         . EventSet.insert f
         . History.lookupReceiver t) c
        ==
        (History.lookupReceiver t
         . History.advanceBy dt
         . History.insert m) c
  where
    f = messageFrom m
    ts = messageTo m

tests_DVars = testGroup "DVars"
        [ testProperty "lookupSender" prop_DVars_lookupSender
        , testProperty "lookupDyad (dual)" prop_DVars_lookupDyad_dual
        ]

prop_DVars_lookupSender (DVarsWithHistory h dv) =
    flip all (History.senders h) $ \s -> let
        rds = DVars.lookupSender h s dv
        in map (second sort) rds
               == [ (r, sort $ DVars.lookupDyad h s r dv)
                  | (r,_) <- rds ]

prop_DVars_lookupDyad_dual (ActorIdList ss) (ActorIdList rs) t0 int =
    forAll (history ss rs t0 dv) $ \h ->
        flip all (History.senders h) $ \s ->
        flip all (History.receivers h) $ \r ->
            (dual . sort) (DVars.lookupDyad h s r dv)
                == sort (DVars.lookupDyad h r s dv)
  where
    dv = DVars.fromIntervals int int

    dual [] = []
    dual [ Send i ] = [ Receive i ]
    dual [ Receive i' ] = [ Send i' ]
    dual [ Send i, Receive i' ] = [ Send i', Receive i ]


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
        == foldl1' addVector [ SVars.lookupDyad f t s | t <- ts ]
    , Summary.dvarsSendSum smry
        == (foldl' (flip $ \i -> Map.insertWith' (+) i 1)
                  Map.empty $ concat $
                  [ mapMaybe DVars.send $ DVars.lookupDyad c f t d
                  | t <- ts ])
    , Summary.dvarsReceiveSum smry
        == (foldl' (flip $ \i -> Map.insertWith' (+) i 1)
                  Map.empty $ concat $
                  [ mapMaybe DVars.receive $ DVars.lookupDyad c f t d
                  | t <- ts ])
    ]
  where
    f = messageFrom m
    ts = messageTo m
    smry = Summary.singleton s d (c,m)

prop_Summary_fromList (MessagesWithVars s d h tms) =
    Summary.fromList s d hms
        == foldl' Summary.union
                  (Summary.empty s d)
                  (map (Summary.singleton s d) hms)
  where
    (_,hms) = History.accum h tms


tests_Model = testGroup "Model"
    [ testProperty "receivers (static)" prop_Model_receivers_static
    , testProperty "receivers" prop_Model_receivers
    , testProperty "sum . probs (static)" prop_Model_sum_probs_static
    , testProperty "sum . probs" prop_Model_sum_probs
    , testProperty "prob (static)" prop_Model_prob_static
    , testProperty "prob" prop_Model_prob
    , testProperty "probParts (static)" prop_Model_prob_parts_static
    , testProperty "probParts" prop_Model_prob_parts    
    , testProperty "exptectedSVars (static)" prop_Model_expectedSVars_static
    , testProperty "exptectedSVars" prop_Model_expectedSVars
    , testProperty "expectedDVars (static)" prop_Model_expectedDVars_static
    , testProperty "expectedDVars" prop_Model_expectedDVars
    ]
    
prop_Model_receivers (SenderModelWithHistory h sm) =
    sort (Model.receivers (Model.receiverModel h sm))
        ==
        sort (Params.receivers s p)
  where
    p = Model.params sm
    s = Model.sender sm

prop_Model_receivers_static (SenderModelWithHistory _ sm) =
    sort (Model.receivers (Model.staticReceiverModel sm))
        ==
        sort (Params.receivers s p)
  where
    p = Model.params sm
    s = Model.sender sm

prop_Model_sum_probs_static (SenderModelWithHistory _ sm) =
    sum (snd $ unzip $ Model.probs $ Model.staticReceiverModel sm) ~== 1

prop_Model_prob_static (SenderModelWithHistory _ sm) =
    flip all (Model.probs $ Model.staticReceiverModel sm) $ \(r,prob) ->
        prob ~== Params.staticProb s r p
  where
    s = Model.sender sm
    p = Model.params sm

prop_Model_sum_probs (SenderModelWithHistory h sm) =
    sum (snd $ unzip $ Model.probs $ Model.receiverModel h sm) ~== 1

prop_Model_prob (SenderModelWithHistory h sm) =
    flip all (Model.probs rm) $ \(r,prob) ->
        prob ~== Params.prob h s r p
  where
    s = Model.sender sm
    rm = Model.receiverModel h sm
    p = Model.params sm

prop_Model_prob_parts_static (SenderModelWithHistory _ sm) =
    flip all (Model.receivers rm) $ \r -> let
        (w0,d) = Model.probParts rm r
        in w0 == 1 && d == 0
  where
    s = Model.sender sm
    rm = Model.staticReceiverModel sm
    p = Model.params sm        

prop_Model_prob_parts (SenderModelWithHistory h sm) =
    flip all (Model.receivers rm) $ \r -> let
        (w0,d) = Model.probParts rm r
        in w0 * Params.staticProb s r p + d
               ~== Params.prob h s r p
  where
    s = Model.sender sm
    rm = Model.receiverModel h sm
    p = Model.params sm        

prop_Model_expectedSVars_static (SenderModelWithHistory _ sm) =
    Model.expectedSVars rm
        ~==
        foldl' (flip $ \(r,w) ->
                    addVectorWithScales w (SVars.lookupDyad s r sv) 1)
               (constantVector (SVars.dim sv) 0)
               (Model.probs rm)
  where
    s = Model.sender sm
    rm = Model.staticReceiverModel sm
    p = Model.params sm
    sv = Params.svars p

prop_Model_expectedSVars (SenderModelWithHistory h sm) =
    Model.expectedSVars rm
        ~==
        Params.expectedSVars h s p
  where
    s = Model.sender sm
    rm = Model.receiverModel h sm
    p = Model.params sm
    sv = Params.svars p

prop_Model_expectedDVars_static (SenderModelWithHistory _ sm) =
    Model.expectedDVars rm == []
  where
    s = Model.sender sm
    rm = Model.staticReceiverModel sm

prop_Model_expectedDVars (SenderModelWithHistory h sm) =
    (sort . filter ((/= 0) . snd)) (Model.expectedDVars rm)
        ~==
        (sort . filter ((/= 0) . snd)) (Params.expectedDVars h s p)
  where
    s = Model.sender sm
    rm = Model.receiverModel h sm
    p = Model.params sm
    dv = Params.dvars p

    
main :: IO ()
main = defaultMain [ tests_SVars
                   , tests_Intervals
                   , tests_EventSet
                   , tests_History
                   , tests_DVars
                   , tests_Summary
                   , tests_Model
                   ]

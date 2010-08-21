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

import EventSet( EventSet )
import qualified EventSet as EventSet

import Intervals(Intervals )
import qualified Intervals as Intervals

import Message

import Vars( Vars )
import qualified Vars as Vars

-- | Times are in [Jan 1, 1970, Feb 1, 1970)
instance Arbitrary UTCTime where
    arbitrary = do
        n <- choose (0, 31 * 24 * 3600 - 1) :: Gen Int
        return $ posixSecondsToUTCTime $ fromIntegral n

-- | DiffTimes are between 0 secs and 1 week
instance Arbitrary NominalDiffTime where
    arbitrary = do
        secs <- oneof [ choose (0, 7 * 24 * 3600)
                      , choose (0, 24 * 3600)
                      , choose (0, 3600)
                      , choose (0, 60)
                      ] :: Gen Int
        return $ fromIntegral secs

-- | Intervals are between 1 sec and ~6 days
data IntervalList = IntervalList [NominalDiffTime] deriving (Show)
instance Arbitrary IntervalList where
    arbitrary = do
        l <- choose (1,5)
        ts <- replicateM l $ elements $ map (fromIntegral . (2^)) [ 0..19 ]
        return $ IntervalList $ nub $ sort ts

data NonEmptyIntervalList = NonEmptyIntervalList [NominalDiffTime] deriving (Show)
instance Arbitrary NonEmptyIntervalList where
    arbitrary = do
        t <- (succ . abs) `fmap` arbitrary
        (IntervalList ts) <- arbitrary
        return $ NonEmptyIntervalList $ nub $ sort (t:ts)

instance Arbitrary Intervals where
    arbitrary = do
        (IntervalList ts) <- arbitrary
        return $ Intervals.fromList ts

data NonEmptyIntervals = NonEmptyIntervals Intervals deriving (Show)
instance Arbitrary NonEmptyIntervals where
    arbitrary = do
        (NonEmptyIntervalList ts) <- arbitrary
        return $ NonEmptyIntervals $ Intervals.fromList ts

data UpdateEventSet e = EventSetAdvance NominalDiffTime
                      | EventSetInsert e
    deriving (Show)
    
updateEventSet :: (Ord e) => UpdateEventSet e -> EventSet e -> EventSet e
updateEventSet (EventSetAdvance dt) = EventSet.advance dt
updateEventSet (EventSetInsert e) = EventSet.insert e
    
instance (Arbitrary e, Ord e, Num e, Random e) => Arbitrary (UpdateEventSet e) where
    arbitrary = do
        dt <- fmap abs arbitrary
        e <- choose (0,5)
        elements [ EventSetAdvance dt, EventSetInsert e]

instance (Arbitrary e, Ord e, Num e, Random e) => Arbitrary (EventSet e) where
    arbitrary = do
        us <- arbitrary
        return $ foldr updateEventSet EventSet.empty us

-- | A list of actors ids (at least two, to avoid self-loop problems)
data ActorIds = ActorIds [ActorId] deriving (Show)
instance Arbitrary ActorIds where
    arbitrary = do
        i1 <- arbitrary
        i2 <- arbitrary >>= \d -> return $ if d == 0 then i1 + 1 else i1 + d
        l <- choose (0,3)
        is <- replicateM l arbitrary
        return $ ActorIds $ nub $ i1:i2:is

data Actors = Actors [SenderId] [ReceiverId] deriving (Show)
instance Arbitrary Actors where
    arbitrary = do
        (ActorIds ids) <- arbitrary
        rsize <- choose (2, length ids)
        ssize <- choose (1, length ids)
        let ss = take ssize ids
            rs = drop (length ids - rsize) ids
        return $ Actors ss rs

data ActorsWithVectors = ActorsWithVectors (Map SenderId (Vector Double))
                                           (Map ReceiverId (Vector Double))
    deriving (Show)
instance Arbitrary ActorsWithVectors where
    arbitrary = do
        (Actors ss rs) <- arbitrary
        p <- choose (0,3)
        q <- choose (0,3)
        xs <- replicateM (length ss) $ Test.vector p
        ys <- replicateM (length rs) $ Test.vector q
        return $ ActorsWithVectors (Map.fromList $ zip ss xs)
                                   (Map.fromList $ zip rs ys)

history :: Actors -> Gen History
history as = do
    ms <- messages as
    t0 <- arbitrary
    ts <- (sort . map (`addUTCTime` t0)) `fmap` replicateM (length ms) arbitrary
    let ((_,h),_) = History.accum (t0, History.empty) $ zip ts ms
    return h

instance Arbitrary History where
    arbitrary = do
        as <- arbitrary
        history as

data VarsWithHistory = VarsWithHistory Vars History deriving (Show)
instance Arbitrary VarsWithHistory where
    arbitrary = do
        (ActorsWithVectors sxs rys) <- arbitrary
        sint <- arbitrary
        rint <- arbitrary
        h <- history (Actors (Map.keys sxs) (Map.keys rys))
        return $ VarsWithHistory (Vars.fromActors sxs rys sint rint) h

message :: Actors -> Gen Message
message = messageWithLoop True
    
messageWithLoop :: Bool -> Actors -> Gen Message
messageWithLoop loop (Actors ss rs) = do
    f <- elements ss
    let rs' = if loop then rs else filter (/= f) rs
    l <- choose (1, length rs')
    ts <- nub `fmap` replicateM l (elements rs')
    return $ Message f ts

messages :: Actors -> Gen [Message]
messages = messagesWithLoops True
    
messagesWithLoops :: Bool -> Actors -> Gen [Message]
messagesWithLoops loops as = sized $ \n -> do
    k <- choose (0,n)
    replicateM k $ messageWithLoop loops as

data MessageWithHistory = MessageWithHistory History Message deriving Show
instance Arbitrary MessageWithHistory where
    arbitrary = do
        as <- arbitrary
        m  <- message as
        h <- history as
        return $ MessageWithHistory h m


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
    int = Intervals.fromList ts
    in and [ Intervals.at i int == t | (i,t) <- zip [ 0.. ] ts ]

prop_Intervals_assocs_fromList (IntervalList ts) =
    (Intervals.assocs . Intervals.fromList) ts == zip [ 0.. ] ts

prop_Intervals_toList_fromList (IntervalList ts) =
    (Intervals.toList . Intervals.fromList) ts == ts
    
prop_Intervals_fromList_toList int =
    (Intervals.fromList . Intervals.toList) int == int
    
prop_Intervals_lookup_empty t =
    Intervals.lookup t Intervals.empty == Nothing

prop_Intervals_lookup_nonpos (NonNegative t) int =
    Intervals.lookup (negate t) int == Nothing
    
prop_Intervals_lookup_endpoint (NonEmptyIntervals int) = let
    in forAll (choose (0,n-1)) $ \i -> let
           t = Intervals.at i int
           in Intervals.lookup t int == Just i
  where
    n = Intervals.size int

prop_Intervals_lookup_before_endpoint (NonEmptyIntervals int) =
    forAll (choose (0,n-1)) $ \i -> let
        t_begin = if i == 0 then 0 else Intervals.at (i-1) int
        t_end = Intervals.at i int
        t = pred t_end
        in Intervals.lookup t int ==
            if t == 0 then Nothing
                      else if t == t_begin then Just (i-1)
                                           else Just i
  where
    n = Intervals.size int

prop_Intervals_lookup_after_endpoint (NonEmptyIntervals int) =
    forAll (choose (0,n-1)) $ \i -> let
        t_begin = if i == 0 then 0 else Intervals.at (i-1) int
        t = succ t_begin
        in Intervals.lookup t int == Just i
  where
    n = Intervals.size int

prop_Intervals_lookup_beyond_last (NonEmptyIntervals int) =
    Intervals.lookup (succ tlast) int == Nothing
  where
    n = Intervals.size int
    tlast = Intervals.at (n - 1) int


tests_EventSet = testGroup "EventSet"
    [ testProperty "current . insert" prop_EventSet_current_insert
    , testProperty "past . advance" prop_EventSet_past_advance
    , testProperty "lookup . advance . insert" prop_EventSet_lookup_advance_insert
    ]
    
prop_EventSet_current_insert h e =
    (sort . EventSet.current . EventSet.insert e) h
    ==
    (sort . nub . (e:) . EventSet.current) h
  where
    _ = h :: EventSet Int
    
prop_EventSet_past_advance h (NonNegative dt)
    | dt == 0 =
        ((EventSet.past . EventSet.advance dt) h)
            `eq`
            (EventSet.past h)
    | otherwise =
        ((EventSet.past . EventSet.advance dt) h)
            `eq`
            (nubBy ((==) `on` fst))
                 (map (\e -> (e,dt)) (EventSet.current h)
                  ++
                  map (\(e,t) -> (e,t+dt)) (EventSet.past h)
                 )
  where
    eq xs ys = sort xs == sort ys
    _ = h :: EventSet Int
    
prop_EventSet_lookup_advance_insert h e (NonNegative dt) =
    (EventSet.lookup e
     . EventSet.advance dt'
     . EventSet.insert e) h
        == Just dt'
  where
    dt' = succ dt
    _ = e :: Int


tests_History = testGroup "History"
    [ testProperty "lookupSender/insert" prop_History_lookupSender_insert
    , testProperty "lookupReceiver/insert" prop_History_lookupReceiver_insert
    , testProperty "lookupSender/lookupReceiver" prop_History_lookupSender_lookupReceiever
    , testProperty "lookupReceiver/lookupSender" prop_History_lookupReceiver_lookupSender
    ]

prop_History_lookupSender_insert (MessageWithHistory h m) (NonNegative dt) =
        (EventSet.advance dt
         . flip (foldr EventSet.insert) ts
         . History.lookupSender f) h
        ==
        (History.lookupSender f
         . History.advance dt
         . History.insert m) h
  where
    f = messageFrom m
    ts = messageTo m

prop_History_lookupReceiver_insert (MessageWithHistory h m) (NonNegative dt) =
    flip all ts $ \t ->
        (EventSet.advance dt
         . EventSet.insert f
         . History.lookupReceiver t) h
        ==
        (History.lookupReceiver t
         . History.advance dt
         . History.insert m) h
  where
    f = messageFrom m
    ts = messageTo m

prop_History_lookupReceiver_lookupSender h =
    and [ and [ Just dt == (EventSet.lookup s $ History.lookupReceiver r h)
              | (r,dt) <- EventSet.past $ History.lookupSender s h
              ]
        | s <- History.senders h
        ]

prop_History_lookupSender_lookupReceiever h =
    and [ and [ Just dt == (EventSet.lookup r $ History.lookupSender s h)
              | (s,dt) <- EventSet.past $ History.lookupReceiver r h
              ]
        | r <- History.receivers h
        ]



tests_Vars = testGroup "Vars"
    [ testProperty "senders . fromActors" prop_Vars_senders_fromActors
    , testProperty "receivers . fromActors" prop_Vars_receivers_fromActors
    , testProperty "dyad . fromActors (static only)" prop_Vars_dyad_fromActors_static
    , testProperty "sender . fromActors (static only)" prop_Vars_sender_fromActors_static
    , testProperty "dyadChanges" prop_Vars_dyadChanges
    , testProperty "senderChanges" prop_Vars_senderChanges
    , testProperty "dyad" prop_Vars_dyad
    , testProperty "sender" prop_Vars_sender
    , testProperty "mulDyadBy" prop_Vars_mulDyadBy
    , testProperty "mulSenderBy" prop_Vars_mulSenderBy
    ]

prop_Vars_senders_fromActors (ActorsWithVectors sxs rys) sint rint =
    Vars.senders (Vars.fromActors sxs rys sint rint)
        == Map.keys sxs

prop_Vars_receivers_fromActors (ActorsWithVectors sxs rys) sint rint =
    Vars.receivers (Vars.fromActors sxs rys sint rint)
        == Map.keys rys

prop_Vars_dyad_fromActors_static (ActorsWithVectors sxs rys) sint rint = let
    v = Vars.fromActors sxs rys sint rint
    h = History.empty
    in and [ Vars.dyad v h s r
                 ===
                 concatVectors [ constantVector d 0
                               , listVector (p*q)
                                            [ xi * yj | yj <- elemsVector y
                                                      , xi <- elemsVector x ]
                               ]
           | (s,x) <- Map.assocs sxs
           , (r,y) <- Map.assocs rys
           ]
  where
    p = (dimVector . snd . Map.findMin) sxs
    q = (dimVector . snd . Map.findMin) rys
    d = Intervals.size sint + Intervals.size rint
    
prop_Vars_sender_fromActors_static (ActorsWithVectors sxs rys) sint rint = let
    v = Vars.fromActors sxs rys sint rint
    h = History.empty
    in and [ Vars.sender v h s
                 ===
                 [ (r, Vars.dyad v h s r) | r <- Vars.receivers v ]
           | s <- Vars.senders v
           ]

prop_Vars_dyadChanges (VarsWithHistory v h) =
    and [ and [ ( if i < ns
                      then (flip Intervals.lookup sint
                               =<< (EventSet.lookup r
                                    $ History.lookupSender s h))
                           == Just i
                      else (flip Intervals.lookup rint
                               =<< (EventSet.lookup s
                                    $ History.lookupSender r h))
                           == Just (i - ns)
                &&
                  e === 1.0
                )
              | (i,e) <- Vars.dyadChanges v h s r
              ]
        | s <- Vars.senders v
        , r <- Vars.receivers v
        ]
  where
    sint = Vars.sendIntervals v
    rint = Vars.receiveIntervals v
    ns = Intervals.size sint

prop_Vars_senderChanges (VarsWithHistory v h) =
    and [ Vars.senderChanges v h s
              ===
              [ (r, Vars.dyadChanges v h s r)
              | r <- Vars.receivers v
              , (not . null) (Vars.dyadChanges v h s r)
              ]
        | s <- Vars.senders v
        ]
    
prop_Vars_dyad (VarsWithHistory v h) =
    and [ Vars.dyad v h s r
              ===
              accumVector (+) (Vars.dyad v h0 s r) (Vars.dyadChanges v h s r)
        | s <- Vars.senders v
        , r <- Vars.receivers v
        ]
  where
    h0 = History.empty

prop_Vars_sender (VarsWithHistory v h) =
    and [ Vars.sender v h s
              ===
              [ (r, Vars.dyad v h s r)
              | r <- Vars.receivers v
              ]
        | s <- Vars.senders v
        ]

prop_Vars_mulDyadBy (VarsWithHistory v h) =
    forAll (Test.vector $ Vars.dim v) $ \beta ->
        and [ Vars.mulDyadBy beta v h s r
                  ~==
                  dotVector (Vars.dyad v h s r) beta
            | s <- Vars.senders v
            , r <- Vars.receivers v
            ]

prop_Vars_mulSenderBy (VarsWithHistory v h) =
    forAll (Test.vector $ Vars.dim v) $ \beta ->
        and [ Vars.mulSenderBy beta v h s
                  ~==
                  [ (r, dotVector x beta) | (r,x) <- Vars.sender v h s ]
            | s <- Vars.senders v
            ]
            
{-

history :: [SenderId] -> [ReceiverId]
        -> UTCTime
        -> DVars
        -> Gen History
history is js t0 dv = do
    n <- choose (0,100)
    dts <- (sort . map (negate . abs)) `fmap` replicateM n arbitrary
    ms <- replicateM n $ message is js
    
    let ts = map (`addUTCTime` t0) dts
        h0 = DVars.history (minimum (t0:ts)) dv
        h = fst $ History.accum h0 $ zip ts ms
        h' = History.advanceTo t0 h
    
    return h'

data DVarsWithHistory = DVarsWithHistory History DVars deriving (Show)
instance Arbitrary DVarsWithHistory where
    arbitrary = do
        dv <- arbitrary
        (ActorIdList is) <- arbitrary
        (ActorIdList js) <- arbitrary
        t0 <- arbitrary
        h <- history is js t0 dv
        return $ DVarsWithHistory h dv

    
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
-}
    
main :: IO ()
main = defaultMain [ tests_Intervals
                   , tests_EventSet
                   , tests_History
                   , tests_Vars
                   -- , tests_Summary
                   -- , tests_Model
                   ]

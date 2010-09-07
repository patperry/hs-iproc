module Main
    where

import Control.Arrow( second )
import Control.Monad( replicateM )
import Data.AEq( AEq(..) )
import Data.Function( on )
import Data.List( foldl', foldl1', nub, nubBy, sort, maximumBy )
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Maybe( fromMaybe, fromJust, catMaybes, mapMaybe )
import qualified Data.Map as Map
import Debug.Trace
import System.Random( Random )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( vector )
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.LinearAlgebra as Test
import Numeric.LinearAlgebra

import History( History )
import EventSet( EventSet )
import Intervals( Intervals )
import LogLik( LogLik )
import Model( Model )
import Types( Actor(..), ActorId, SenderId, ReceiverId, Message(..),
    Time, DiffTime, posixSecondsToTime, addTime )
import Vars( Vars )

import qualified History as History
import qualified EventSet as EventSet
import qualified Intervals as Intervals
import qualified LogLik as LogLik
import qualified Model as Model
import qualified Vars as Vars


verboseEq a b | a === b = True
              | otherwise =
    trace ("\nexpected: " ++ show b ++ "\nactual: " ++ show a) False

verboseAEq a b | a ~== b = True
              | otherwise =
    trace ("\nexpected: " ++ show b ++ "\nactual: " ++ show a) False


-- | Times are in [Jan 1, 1970, Feb 1, 1970)
instance Arbitrary Time where
    arbitrary = do
        n <- choose (0, 31 * 24 * 3600 - 1) :: Gen Int
        return $ posixSecondsToTime $ fromIntegral n

-- | DiffTimes are between 0 secs and 1 week
instance Arbitrary DiffTime where
    arbitrary = do
        secs <- oneof [ choose (0, 7 * 24 * 3600)
                      , choose (0, 24 * 3600)
                      , choose (0, 3600)
                      , choose (0, 60)
                      ] :: Gen Int
        return $ fromIntegral secs

-- | Intervals are between 1 sec and ~6 days
data IntervalList = IntervalList [DiffTime] deriving (Show)
instance Arbitrary IntervalList where
    arbitrary = do
        l <- choose (1,5)
        ts <- replicateM l $ elements $ map (fromIntegral . (2^)) [ 0..19 ]
        return $ IntervalList $ nub $ sort ts

data NonEmptyIntervalList = NonEmptyIntervalList [DiffTime] deriving (Show)
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

data UpdateEventSet e = EventSetAdvance DiffTime
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
        xs <- replicateM (length ss) $ testVector p
        ys <- replicateM (length rs) $ testVector q
        return $ ActorsWithVectors (Map.fromList $ zip ss xs)
                                   (Map.fromList $ zip rs ys)
      where
        testVector p = listVector p `fmap` replicateM p (choose (-1,1))

instance Arbitrary Vars where
    arbitrary = do
        (ActorsWithVectors sxs rys) <- arbitrary
        sint <- arbitrary
        rint <- arbitrary
        return $ Vars.fromActors sxs rys sint rint

history :: [SenderId] -> [ReceiverId] -> Gen History
history ss rs = do
    ms <- messages ss rs
    t0 <- arbitrary
    ts <- (sort . map (`addTime` t0)) `fmap` replicateM (length ms) arbitrary
    let ((_,h),_) = History.accum (t0, History.empty) $ zip ts ms
    return h

instance Arbitrary History where
    arbitrary = do
        (Actors ss rs) <- arbitrary
        history ss rs

data VarsWithHistory = VarsWithHistory Vars History deriving (Show)
instance Arbitrary VarsWithHistory where
    arbitrary = do
        v <- arbitrary
        h <- history (Vars.senders v) (Vars.receivers v)
        return $ VarsWithHistory v h

message :: [SenderId] -> [ReceiverId] -> Gen Message
message = messageWithLoop True
    
messageWithLoop :: Bool -> [SenderId] -> [ReceiverId] -> Gen Message
messageWithLoop loop ss rs = do
    f <- elements ss
    let rs' = if loop then rs else filter (/= f) rs
    l <- choose (1, length rs')
    ts <- nub `fmap` replicateM l (elements rs')
    return $ Message f ts

messages :: [SenderId] -> [ReceiverId] -> Gen [Message]
messages = messagesWithLoops True
    
messagesWithLoops :: Bool -> [SenderId] -> [ReceiverId] -> Gen [Message]
messagesWithLoops loops ss rs = sized $ \n -> do
    k <- choose (0,n)
    replicateM k $ messageWithLoop loops ss rs

data MessageWithHistory = MessageWithHistory Message History deriving Show
instance Arbitrary MessageWithHistory where
    arbitrary = do
        (Actors ss rs) <- arbitrary
        m  <- message ss rs
        h <- history ss rs
        return $ MessageWithHistory m h

instance Arbitrary Model where
    arbitrary = do
        v <- arbitrary
        let p = Vars.dim v
        c <- listVector p `fmap` replicateM p (choose (-1,1))
        l <- elements [ Model.Loops, Model.NoLoops ]
        return $ Model.fromVars v c l

data ModelWithHistory = ModelWithHistory Model History deriving Show
instance Arbitrary ModelWithHistory where
    arbitrary = do
        m <- arbitrary
        h <- history (Model.senders m) (Model.receivers m)
        return $ ModelWithHistory m h

messageFromModel :: Model -> Gen Message
messageFromModel m =
    messageWithLoop (Model.hasLoops m) (Model.senders m) (Model.receivers m)

data ModelWithMessage = ModelWithMessage Model Message deriving Show
instance Arbitrary ModelWithMessage where
    arbitrary = do
        m <- arbitrary
        msg <- messageFromModel m
        return $ ModelWithMessage m msg

data ModelWithMessageAndHistory =
    ModelWithMessageAndHistory Model (Message,History) deriving Show
instance Arbitrary ModelWithMessageAndHistory where
    arbitrary = do
        (ModelWithHistory m h) <- arbitrary
        msg <- messageFromModel m
        return $ ModelWithMessageAndHistory m (msg,h)

data ModelWithMessage2 = ModelWithMessage2 Model Message Message deriving Show
instance Arbitrary ModelWithMessage2 where
    arbitrary = do
        m <- arbitrary
        msg1 <- messageFromModel m
        msg2 <- messageFromModel m        
        return $ ModelWithMessage2 m msg1 msg2

data ModelWithMessageAndHistory2 =
    ModelWithMessageAndHistory2 Model (Message,History) (Message,History)
    deriving Show
instance Arbitrary ModelWithMessageAndHistory2 where
    arbitrary = do
        m <- arbitrary
        h1 <- history (Model.senders m) (Model.receivers m)
        h2 <- history (Model.senders m) (Model.receivers m)
        msg1 <- messageFromModel m
        msg2 <- messageFromModel m
        return $ ModelWithMessageAndHistory2 m (msg1,h1) (msg2,h2)



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

prop_History_lookupSender_insert (MessageWithHistory m h) (NonNegative dt) =
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

prop_History_lookupReceiver_insert (MessageWithHistory m h) (NonNegative dt) =
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
    , testProperty "weightReceiverBy" prop_Vars_weightReceiverBy
    , testProperty "weightReceiverChangesBy" prop_Vars_weightReceiverChangesBy
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

prop_Vars_weightReceiverBy (VarsWithHistory v h) =
    forAll (sized $ \s -> do
                k <- choose (0,s)
                rs <- replicateM k (elements $ Vars.receivers v)
                ws <- replicateM k arbitrary
                return $ zip rs ws) $ \rws ->
    flip all (Vars.senders v) $ \s ->
        Vars.weightReceiverBy rws v h s
            `verboseAEq`
            weightedSumVector (Vars.dim v)
                [ (w, Vars.dyad v h s r) | (r,w) <- rws ]

prop_Vars_weightReceiverChangesBy (VarsWithHistory v h) =
    forAll (sized $ \s -> do
                k <- choose (0,s)
                rs <- replicateM k (elements $ Vars.receivers v)
                ws <- replicateM k arbitrary
                return $ zip rs ws) $ \rws ->
    flip all (Vars.senders v) $ \s ->
    flip all (assocsVector $ Vars.weightReceiverBy rws v h s) $ \(i,d) ->
        let x0 = Vars.weightReceiverBy rws v History.empty s
        in case (lookup i (Vars.weightReceiverChangesBy rws v h s)) of
              Just d' -> d ~== d' + atVector x0 i
              Nothing -> d ~== atVector x0 i

tests_Model = testGroup "Model"
    [ testProperty "logWeight (static)" prop_Model_logWeight_static
    , testProperty "logWeight" prop_Model_logWeight
    , testProperty "logWeightChange" prop_Model_logWeightChange
    , testProperty "logWeightChanges" prop_Model_logWeightChanges
    , testProperty "logWeights" prop_Model_logWeights
    , testProperty "weight" prop_Model_weight
    , testProperty "weights" prop_Model_weights
    , testProperty "sumWeights (static)" prop_Model_sumWeights_static
    , testProperty "sumWeights" prop_Model_sumWeights    
    , testProperty "logSumWeights (static)" prop_Model_logSumWeights_static
    , testProperty "logSumWeights" prop_Model_logSumWeights    
    , testProperty "probs (static)" prop_Model_probs_static
    , testProperty "probs" prop_Model_probs
    , testProperty "sum . probs (static)" prop_Model_sum_probs_static
    , testProperty "sum . probs" prop_Model_sum_probs
    , testProperty "prob (static)" prop_Model_prob_static
    , testProperty "prob" prop_Model_prob
    , testProperty "meanVars (static)" prop_Model_meanVars_static
    , testProperty "meanVars" prop_Model_meanVars
    , testProperty "covVars (static)" prop_Model_covVars_static    
    , testProperty "covVars" prop_Model_covVars
    ]

prop_Model_logWeight_static m =
    and [ Model.logWeight m h s r
             ~==
             if Model.validDyad m s r
                 then Vars.mulDyadBy beta v h s r
                 else neginf 
        | s <- Model.senders m
        , r <- Model.receivers m
        ]
  where
    beta = Model.coefs m
    v = Model.vars m
    h = History.empty
    neginf = -1/0 :: Double

prop_Model_logWeight (ModelWithHistory m h) =
    and [ Model.logWeight m h s r
             ~==
             if Model.validDyad m s r
                 then Vars.mulDyadBy beta v h s r
                 else neginf 
        | s <- Model.senders m
        , r <- Model.receivers m
        ]
  where
    beta = Model.coefs m
    v = Model.vars m
    neginf = -1/0 :: Double

prop_Model_logWeightChange (ModelWithHistory m h) =
    and [ if not (Model.validDyad m s r)
              then Model.logWeightChange m h s r === 0
              else (Model.logWeightChange m h s r + Model.logWeight m h0 s r)
                   `verboseAEq` Model.logWeight m h s r
        | s <- Model.senders m
        , r <- Model.receivers m
        ]
  where
    h0 = History.empty

prop_Model_logWeightChanges (ModelWithHistory m h) =
    and [ fromMaybe 0 (lookup r $ Model.logWeightChanges m h s)
              ~==
              Model.logWeightChange m h s r
        | s <- Model.senders m
        , r <- Model.validReceivers m s
        ]

prop_Model_logWeights (ModelWithHistory m h) =
    and [ Model.logWeights m h s
            ~== [ (r, Model.logWeight m h s r)
                | r <- Model.validReceivers m s
                ]
        | s <- Model.senders m
        ]

prop_Model_weight (ModelWithHistory m h) =
    and [ Model.weight m h s r
             ~==
             exp (Model.logWeight m h s r)
        | s <- Model.senders m
        , r <- Model.receivers m
        ]
  where
    h0 = History.empty

prop_Model_weights (ModelWithHistory m h) =
    and [ Model.weights m h s
            ~== [ (r, Model.weight m h s r)
                | r <- Model.validReceivers m s
                ]
        | s <- Model.senders m
        ]

prop_Model_sumWeights_static m =
    and [ Model.sumWeights m h s ~== (sum . snd . unzip) (Model.weights m h s)
        | s <- Model.senders m
        ]
  where
    h = History.empty

prop_Model_sumWeights (ModelWithHistory m h) =
    and [ Model.sumWeights m h s ~== (sum . snd . unzip) (Model.weights m h s)
        | s <- Model.senders m
        ]

prop_Model_logSumWeights_static m =
    flip all (Model.senders m) $ \s ->
    flip all (Model.receivers m) $ \r ->
        (Model.logWeight m h s r)
            `verboseAEq` (Model.logProb m h s r + Model.logSumWeights m h s)
  where
    h = History.empty

prop_Model_logSumWeights (ModelWithHistory m h) =
    flip all (Model.senders m) $ \s ->
    flip all (Model.receivers m) $ \r ->
        (Model.logWeight m h s r)
            `verboseAEq` (Model.logProb m h s r + Model.logSumWeights m h s)

prop_Model_probs_static m =
    and [ and [ Model.prob m History.empty s r
                    `verboseAEq`
                    fromMaybe 0 (lookup r (Model.probs m History.empty s))
              | r <- Model.receivers m
              ]
        | s <- Model.senders m
        ]

prop_Model_probs (ModelWithHistory m h) =
    and [ and [ Model.prob m h s r
                    `verboseAEq`
                    fromMaybe 0 (lookup r (Model.probs m h s))
              | r <- Model.receivers m
              ]
        | s <- Model.senders m
        ]

prop_Model_sum_probs_static m =
    and [ (sum . snd . unzip) (Model.probs m History.empty s) ~== 1
        | s <- Model.senders m
        , (not . null) (Model.probs m History.empty s)
        ]

prop_Model_sum_probs (ModelWithHistory m h) =
    and [ (sum . snd . unzip) (Model.probs m h s) ~== 1
        | s <- Model.senders m
        , (not . null) (Model.probs m h s)
        ]

prop_Model_prob_static m =
    and [ and [ (Model.prob m h s r / Model.prob m h s r0)
                    `verboseAEq`
                    exp (Model.logWeight m h s r - Model.logWeight m h s r0)
              | r <- Model.validReceivers m s
              ]
        | s <- Model.senders m
        , (not . null) (Model.validReceivers m s)
        , (r0,_) <- [ maximumBy (compare `on` snd) $ Model.probs m h s ]
        ]
  where
    h = History.empty    

prop_Model_prob (ModelWithHistory m h) =
    and [ and [ (Model.prob m h s r / Model.prob m h s r0)
                    `verboseAEq`
                    exp (Model.logWeight m h s r - Model.logWeight m h s r0)
              | r <- Model.validReceivers m s
              ]
        | s <- Model.senders m
        , (not . null) (Model.validReceivers m s)
        , (r0,_) <- [ maximumBy (compare `on` snd) $ Model.probs m h s ]
        ]

prop_Model_meanVars_static m =
    flip all (Model.senders m) $ \s ->
        Model.meanVars m h s
            `verboseAEq`
            weightedMeanVector (Vars.dim v)
                               [ (Model.prob m h s r, x)
                               | (r,x) <- Vars.sender v h s
                               ]
  where
    h = History.empty
    v = Model.vars m

prop_Model_meanVars (ModelWithHistory m h) =
    flip all (Model.senders m) $ \s ->
        Model.meanVars m h s
            `verboseAEq`
            weightedMeanVector (Vars.dim v)
                               [ (Model.prob m h s r, x)
                               | (r,x) <- Vars.sender v h s
                               ]
  where
    v = Model.vars m

prop_Model_covVars_static m =
    forAll (Test.vector $ Vars.dim v) $ \z ->
    flip all (Model.senders m) $ \s ->
        let cov  = Model.covVars m h s
            cov' = weightedCovMatrix (Vars.dim v) MLCov
                                    [ (Model.prob m h s r, x)
                                    | (r,x) <- Vars.sender v h s
                                    ]
        in
            mulHermMatrixVector cov z
                ~== mulHermMatrixVector cov' z
  where
    h = History.empty
    v = Model.vars m

prop_Model_covVars (ModelWithHistory m h) =
    forAll (Test.vector $ Vars.dim v) $ \z ->
    flip all (Model.senders m) $ \s ->
        let cov  = Model.covVars m h s
            cov' = weightedCovMatrix (Vars.dim v) MLCov
                                    [ (Model.prob m h s r, x)
                                    | (r,x) <- Vars.sender v h s
                                    ]
        in
            mulHermMatrixVector cov z
                ~== mulHermMatrixVector cov' z
  where
    v = Model.vars m


tests_LogLik = testGroup "LogLik"
    [ testProperty "value . singleton (static)" prop_LogLik_singleton_value_static
    , testProperty "value . doubleton (static)" prop_LogLik_doubleton_value_static
    , testProperty "value . singleton" prop_LogLik_singleton_value
    , testProperty "value . doubleton" prop_LogLik_doubleton_value
    , testProperty "grad . singleton (static)" prop_LogLik_singleton_grad_static
    , testProperty "grad . singleton" prop_LogLik_singleton_grad
    , testProperty "grad . doubleton (static)" prop_LogLik_doubleton_grad_static
    , testProperty "grad . doubleton" prop_LogLik_doubleton_grad
    ]
    
prop_LogLik_singleton_value_static (ModelWithMessage m msg) = let
    val  = LogLik.value $ LogLik.fromMessages m [(msg,h)]
    val' = sum [ Model.logProb m h s r | r <- rs ]
    in if abs val' > eps
            then val `verboseAEq` val'
            else abs val <= eps
  where
    h = History.empty
    s = messageFrom msg
    rs = messageTo msg
    eps = 1e-7

prop_LogLik_doubleton_value_static (ModelWithMessage2 m msg1 msg2) = let
    val1 = LogLik.value $ LogLik.fromMessages m [(msg1,h)]
    val2 = LogLik.value $ LogLik.fromMessages m [(msg2,h)]
    val = LogLik.value $ LogLik.fromMessages m [(msg1,h), (msg2,h)]
    in val ~== val1 + val2
  where
    h = History.empty

prop_LogLik_singleton_value (ModelWithMessageAndHistory m (msg,h)) = let
    val  = LogLik.value $ LogLik.fromMessages m [(msg,h)]
    val' = sum [ Model.logProb m h s r | r <- rs ]
    in if abs val' > eps
            then val `verboseAEq` val'
            else abs val <= eps
  where
    s = messageFrom msg
    rs = messageTo msg
    eps = 1e-7

prop_LogLik_doubleton_value (ModelWithMessageAndHistory2 m mh1 mh2) = let
    val1 = LogLik.value $ LogLik.fromMessages m [ mh1 ]
    val2 = LogLik.value $ LogLik.fromMessages m [ mh2 ]
    val = LogLik.value $ LogLik.fromMessages m [ mh1, mh2 ]
    in val ~== val1 + val2

prop_LogLik_singleton_grad_static (ModelWithMessage m msg) =
    LogLik.grad (LogLik.fromMessages m [(msg,h)])
        ~== sumVector (Vars.dim v) [ Vars.dyad v h s r `subVector` mu
                                   | r <- rs 
                                   ]
  where
    h = History.empty
    s = messageFrom msg
    rs = messageTo msg
    v = Model.vars m
    mu = Model.meanVars m h s

prop_LogLik_doubleton_grad_static (ModelWithMessage2 m msg1 msg2) = let
    grad1 = LogLik.grad $ LogLik.fromMessages m [(msg1,h)]
    grad2 = LogLik.grad $ LogLik.fromMessages m [(msg2,h)]
    grad = LogLik.grad $ LogLik.fromMessages m [(msg1,h), (msg2,h)]
    in grad ~== grad1 `addVector` grad2
  where
    h = History.empty

prop_LogLik_singleton_grad (ModelWithMessageAndHistory m (msg,h)) =
    LogLik.grad (LogLik.fromMessages m [(msg,h)])
        ~== sumVector (Vars.dim v) [ Vars.dyad v h s r `subVector` mu
                                   | r <- rs 
                                   ]
  where
    s = messageFrom msg
    rs = messageTo msg
    v = Model.vars m
    mu = Model.meanVars m h s

prop_LogLik_doubleton_grad =
    forAll (resize 2 $ arbitrary) $ \(ModelWithMessageAndHistory2 m mh1 mh2) -> let
        ll1 = LogLik.fromMessages m [ mh1 ]
        ll2 = LogLik.fromMessages m [ mh2 ]
        ll  = LogLik.fromMessages m [ mh1, mh2 ]
        grad1 = LogLik.grad ll1
        grad2 = LogLik.grad ll2
        grad = LogLik.grad ll
        in if grad ~== (grad1 `addVector` grad2) then True
                else trace ("\nll1: " ++ show ll1
                            ++ "\nll2: " ++ show ll2
                            ++ "\nll: " ++ show ll) False

{-
prop_LogLik_doubleton_grad (ModelWithMessageAndHistory2 m mh1 mh2) = let
    grad1 = LogLik.grad $ LogLik.fromMessages m [ mh1 ]
    grad2 = LogLik.grad $ LogLik.fromMessages m [ mh2 ]
    grad = LogLik.grad $ LogLik.fromMessages m [ mh1, mh2 ]
    in grad `verboseAEq` (grad1 `addVector` grad2)
-}


main :: IO ()
main = defaultMain [ tests_Intervals
                   , tests_EventSet
                   , tests_History
                   , tests_Vars
                   , tests_Model
                   , tests_LogLik
                   ]

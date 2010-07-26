module Properties
    where

import Data.Function( on )
import Data.List( sort, sortBy )
import Data.Maybe( fromJust )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( vector )
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.LinearAlgebra as Test
import Numeric.LinearAlgebra

import Actor( Actor(..) )
import ActorSet( ActorSet )
import qualified ActorSet as ActorSet
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
    in and [ ActorSet.at i a_set == Just a
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

main :: IO ()
main = defaultMain [ tests_ActorSet
                   , tests_SVars
                   ]

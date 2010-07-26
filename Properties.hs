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

import Actor( Actor(..) )
import ActorSet( ActorSet )
import qualified ActorSet as ActorSet

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
    n <- Test.dim
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

data UniqueActorList = UniqueActorList [Actor] deriving (Show)
instance Arbitrary UniqueActorList where
    arbitrary = do
        as <- ActorSet.toList `fmap` arbitrary
        is <- QC.vector $ length as :: Gen [Int]
        let as' = (snd . unzip . sortBy (compare `on` fst)) $ zip is as
        return $ UniqueActorList as'

tests_ActorSet = testGroup "ActorSet"
    [ testProperty "size . fromList" prop_ActorSet_size_fromList
    , testProperty "lookup . fromList" prop_ActorSet_lookup_fromList
    , testProperty "at . fromList" prop_ActorSet_at_fromList
    , testProperty "assocs . fromList" prop_ActorSet_assocs_fromList
    , testProperty "toList . fromList" prop_ActorSet_toList_fromList
    ]

prop_ActorSet_size_fromList (UniqueActorList as) =
    (ActorSet.size . ActorSet.fromList) as == length as
    
prop_ActorSet_lookup_fromList (UniqueActorList as) = let
    a_set = ActorSet.fromList as
    in and [ ActorSet.lookup (actorId a) a_set == Just i
           | (i,a) <- zip [ 0.. ] as 
           ]

prop_ActorSet_at_fromList (UniqueActorList as) = let
    a_set = ActorSet.fromList as
    in and [ ActorSet.at i a_set == Just a
           | (i,a) <- zip [ 0.. ] as 
           ]
           
prop_ActorSet_assocs_fromList (UniqueActorList as) =
    (ActorSet.assocs . ActorSet.fromList) as == zip [ 0.. ] as

prop_ActorSet_toList_fromList (UniqueActorList as) =
    (ActorSet.toList . ActorSet.fromList) as == as

main :: IO ()
main = defaultMain [ tests_ActorSet
                   ]

module T3andT4Spec where

import qualified Data.Set as Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import HW1.T3 (Tree (..), tFromList, tdepth, tinsert, tmember, tsize)
import HW1.T4 (tfoldr)

sorted :: Ord a => [a] -> Bool
sorted []  = True
sorted [x] = True
sorted (x:xs@(y:_))
    | x <= y    = sorted xs
    | otherwise = False

unique :: Ord a => [a] -> Bool
unique xs = helper Set.empty xs
    where helper s [] = True
          helper s (x:xs)
            | Set.member x s = False
            | otherwise      = helper (Set.insert x s) xs

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []

genList :: Gen [Int]
genList = Gen.list (Range.linear 0 10000) Gen.enumBounded

prop_size :: Property
prop_size = property $ do
    xs <- forAll genList
    let tree = tFromList xs
        set  = Set.fromList xs
    tsize tree === Set.size set

propertySize :: TestTree
propertySize = testProperty "tree size" prop_size

prop_sorted :: Property
prop_sorted = property $ do
    xs <- forAll genList
    assert $ sorted ((treeToList . tFromList) xs)

propertySorted :: TestTree
propertySorted = testProperty "tree sorted" prop_sorted

prop_unique :: Property
prop_unique = property $ do
    xs <- forAll genList
    assert $ unique ((treeToList . tFromList) xs)

propertyUnique :: TestTree
propertyUnique = testProperty "tree unique" prop_unique

prop_tFromList :: Property
prop_tFromList = property $ do
    xs <- forAll genList
    (treeToList . tFromList) xs === Set.toAscList (Set.fromList xs)

propertyTFromList :: TestTree
propertyTFromList = testProperty "tree tFromList" prop_tFromList


tests :: TestTree
tests = testGroup "Basic"
    [
        propertySize, propertySorted, propertyUnique, propertyTFromList
    ]

-- ADVANCED --

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Branch _ l _ r)
    | abs (tdepth l - tdepth r) <= 1 = isBalanced l && isBalanced r
    | otherwise                      = False

prop_balanced :: Property
prop_balanced = property $ do
    xs <- forAll genList
    let tree = tFromList xs
    assert $ isBalanced tree

propertyBalanced :: TestTree
propertyBalanced = testProperty "tree balanced" prop_balanced


advancedTests :: TestTree
advancedTests = testGroup "Advanced"
    [
        propertyBalanced
    ]    

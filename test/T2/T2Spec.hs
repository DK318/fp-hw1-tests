module T2Spec where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

import GHC.Natural (Natural)
import HW1.T2 (N (..), nEven, nFromNatural, nOdd, nToNum, ncmp, ndiv, nmod, nmult, nplus, nsub)

nFromNatural' :: Natural -> N
nFromNatural' 0 = Z
nFromNatural' n = S $ nFromNatural' (n - 1)

nToNum' :: Num a => N -> a
nToNum' Z     = 0
nToNum' (S n) = 1 + nToNum' n

genNatural :: Gen Natural
genNatural = Gen.integral_ $ Range.linear 0 100

prop_nplus :: Property
prop_nplus = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    let an = nFromNatural' a
        bn = nFromNatural' b
    nToNum' (nplus an bn) === a + b

propertyNplus :: TestTree
propertyNplus = testProperty "nplus property" prop_nplus

prop_nmult :: Property
prop_nmult = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    let an = nFromNatural' a
        bn = nFromNatural' b
    nToNum' (nmult an bn) === a * b

propertyNmult :: TestTree
propertyNmult = testProperty "nmult property" prop_nmult

prop_nsub :: Property
prop_nsub = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    let an = nFromNatural' a
        bn = nFromNatural' b
    case nsub an bn of
        Just cn -> nToNum' cn === a - b
        Nothing -> assert $ a < b

propertyNsub :: TestTree
propertyNsub = testProperty "nsub property" prop_nsub

prop_ncmp :: Property
prop_ncmp = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    let an = nFromNatural' a
        bn = nFromNatural' b
    ncmp an bn === compare a b

propertyNcmp :: TestTree
propertyNcmp = testProperty "ncmp property" prop_ncmp

prop_nFromNatural :: Property
prop_nFromNatural = property $ do
    n <- forAll genNatural
    nToNum' (nFromNatural n) === n

propertyNFromNatural :: TestTree
propertyNFromNatural = testProperty "nFromNatural property" prop_nFromNatural

prop_nToNum :: Property
prop_nToNum = property $ do
    n <- forAll genNatural
    let nn = nFromNatural' n
    nToNum nn === nToNum' nn

propertyNToNum :: TestTree
propertyNToNum = testProperty "nToNum property" prop_nToNum

-- ADVANCED --

prop_nEven :: Property
prop_nEven = property $ do
    n <- forAll genNatural
    let nn = nFromNatural' n
    nEven nn === even n

propertyNEven :: TestTree
propertyNEven = testProperty "nEven property" prop_nEven

prop_nOdd :: Property
prop_nOdd = property $ do
    n <- forAll genNatural
    let nn = nFromNatural' n
    nOdd nn === odd n

propertyNOdd :: TestTree
propertyNOdd = testProperty "nOdd property" prop_nOdd

prop_ndiv :: Property
prop_ndiv = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    if b == 0 then discard
    else do
        let an = nFromNatural' a
            bn = nFromNatural' b
        nToNum' (ndiv an bn) === a `div` b

propertyNDiv :: TestTree
propertyNDiv = testProperty "nDiv property" prop_ndiv

prop_nmod :: Property
prop_nmod = property $ do
    a <- forAll genNatural
    b <- forAll genNatural
    if b == 0 then discard
    else do
        let an = nFromNatural' a
            bn = nFromNatural' b
        nToNum' (nmod an bn) === a `mod` b

propertyNMod :: TestTree
propertyNMod = testProperty "nMod property" prop_nmod

tests :: IO TestTree
tests = return $ testGroup "HW1.T2"
    [
        propertyNplus, propertyNmult, propertyNsub,
        propertyNcmp, propertyNFromNatural, propertyNToNum,
        propertyNEven, propertyNOdd, propertyNDiv, propertyNMod
    ]

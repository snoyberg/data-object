{-# LANGUAGE FlexibleInstances #-}

import Data.Object

import Test.Framework (defaultMain, testGroup, Test)
--import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
--import Test.HUnit hiding (Test)
import Test.QuickCheck

main :: IO ()
main = defaultMain
    [ testSuite
    ]

testSuite :: Test
testSuite = testGroup "Data.Object"
    [ testProperty "propMapKeysValuesId" propMapKeysValuesId
    , testProperty "propToFromRawObject" propToFromRawObject
    ]

propMapKeysValuesId :: Object Int Int -> Bool
propMapKeysValuesId o = mapKeysValues id id o == o

propToFromRawObject :: Object Int Int -> Bool
propToFromRawObject o = fromRawObject (toRawObject o) == Just o

instance Arbitrary (Object Int Int) where
    coarbitrary = undefined
    arbitrary = oneof [arbS, arbL, arbM] where
        arbS = Scalar `fmap` (arbitrary :: Gen Int)
        arbL = Sequence `fmap` vector 2
        arbM = Mapping `fmap` vector 1

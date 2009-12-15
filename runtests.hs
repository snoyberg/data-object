{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Object.Text
import Data.Attempt
import Data.Convertible.Text

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
    , testProperty "propToFromTextObject" propToFromTextObject
    , testProperty "propStrings" propStrings
    ]

propMapKeysValuesId :: Object Int Int -> Bool
propMapKeysValuesId o = mapKeysValues id id o == o

instance FromObject (Object Int Int) Text Text where
    fromObject = mapKeysValuesM convertAttempt convertAttempt
instance ToObject (Object Int Int) Text Text where
    toObject = mapKeysValues convertSuccess convertSuccess

propToFromTextObject :: Object Int Int -> Bool
propToFromTextObject o = fa (fromTextObject (toTextObject o)) == Just o

instance Arbitrary (Object Int Int) where
    coarbitrary = undefined
    arbitrary = oneof [arbS, arbL, arbM] where
        arbS = Scalar `fmap` (arbitrary :: Gen Int)
        arbL = Sequence `fmap` vector 2
        arbM = Mapping `fmap` vector 1

instance Arbitrary Char where
    coarbitrary = undefined
    arbitrary = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

propStrings :: String -> Bool
propStrings s = fa (fromTextObject $ toTextObject s) == Just s

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Text
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
---------------------------------------------------------

-- | Keys and values are lazy 'Text's.
module Data.Object.Text
    ( TextObject
    , toTextObject
    , fromTextObject
    , Text
    , module Data.Object.Base
#if TEST
    , testSuite
#endif
    ) where

import Data.Object.Base
import Data.Text.Lazy (Text)
import Data.Attempt

import Data.Convertible.Text

import Data.Time.Calendar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Control.Arrow ((***))
#endif

-- | 'Object's with keys and values of type 'Text'.
type TextObject = Object Text Text

-- | 'toObject' specialized for 'TextObject's
toTextObject :: ToObject a Text Text => a -> TextObject
toTextObject = toObject

-- | 'fromObject' specialized for 'TextObject's
fromTextObject :: FromObject a Text Text => TextObject -> Attempt a
fromTextObject = fromObject

instance ToObject (Object String String) Text Text where
    toObject = convertObject

instance ToObject String Text Text where
    toObject = sTO
instance ToObject Day Text Text where
    toObject = sTO
instance ToObject Int Text Text where
    toObject = sTO
instance ToObject Rational Text Text where
    toObject = sTO
instance ToObject Bool Text Text where
    toObject = sTO

instance FromObject String Text Text where
    fromObject = sFO
instance FromObject Day Text Text where
    fromObject = sFO
instance FromObject Int Text Text where
    fromObject = sFO
instance FromObject Rational Text Text where
    fromObject = sFO
instance FromObject Bool Text Text where
    fromObject = sFO

instance ToObject BL.ByteString Text Text where
    toObject = sTO
instance FromObject BL.ByteString Text Text where
    fromObject = sFO

instance ToObject BS.ByteString Text Text where
    toObject = sTO
instance FromObject BS.ByteString Text Text where
    fromObject = sFO

#if TEST
testSuite :: Test
testSuite = testGroup "Data.Object.Text"
    [ testProperty "propMapKeysValuesId" propMapKeysValuesId
    , testProperty "propToFromTextObject" propToFromTextObject
    , testProperty "propStrings" propStrings
    , testCase "autoScalar" autoScalar
    , testCase "autoMapping" autoMapping
    ]

propMapKeysValuesId :: Object Int Int -> Bool
propMapKeysValuesId o = mapKeysValues id id o == o

-- FIXME consider making something automatic, though unlikely
instance FromObject (Object Int Int) Text Text where
    fromObject = convertObjectM
instance ToObject (Object Int Int) Text Text where
    toObject = convertObject

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
propStrings s = fa (sFO $ (sTO s :: TextObject)) == Just s

autoScalar :: Assertion
autoScalar = do
    let t :: Text
        t = cs "This is some text"
    Scalar t @=? toTextObject t

autoMapping :: Assertion
autoMapping = do
    let dummy = [("foo", "FOO"), ("bar", "BAR"), ("five", "5")]
        expected :: TextObject
        expected = Mapping $ map (cs *** Scalar . cs) dummy
    let test' :: (ConvertSuccess String a,
                  ConvertSuccess a Text,
                  ConvertSuccess Text a,
                  FromObject a Text Text,
                  Eq a,
                  Show a)
             => a -> Assertion
        test' a = do
            let dummy' = map (cs *** cs) dummy `asTypeOf` [(a, a)]
                dummy'' = mTO dummy' :: TextObject
            dummy'' @?= expected
            Just dummy' @=? fa (omFO expected)
    test' (undefined :: String)
    test' (undefined :: Text)
    test' (undefined :: BS.ByteString)
    test' (undefined :: BL.ByteString)
#endif

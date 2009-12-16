{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.Time.Calendar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Control.Arrow ((***))
import Data.Convertible.Text
#endif

-- | 'Object's with keys and values of type 'Text'.
type TextObject = Object Text Text

-- | 'convertSuccess' specialized for 'TextObject's
toTextObject :: ConvertSuccess a TextObject => a -> TextObject
toTextObject = cs

-- | 'convertAttempt' specialized for 'TextObject's
fromTextObject :: ConvertAttempt TextObject a => TextObject -> Attempt a
fromTextObject = ca

$(deriveSuccessConvs ''Text ''Text
    [''Text, ''String, ''BS.ByteString, ''BL.ByteString, ''TS.Text]
    [''String, ''Day, ''Int, ''Rational, ''Bool, ''BS.ByteString,
     ''BL.ByteString, ''TS.Text, ''Text
    ])

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
instance ConvertAttempt TextObject (Object Int Int) where
    convertAttempt = convertObjectM
instance ConvertSuccess (Object Int Int) TextObject where
    convertSuccess = convertObject

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
                  Eq a,
                  Show a,
                  ConvertSuccess a TextObject,
                  ConvertAttempt TextObject a,
                  ConvertSuccess [a] TextObject,
                  ConvertAttempt TextObject [a],
                  ConvertSuccess [(a, a)] TextObject,
                  ConvertAttempt TextObject [(a, a)],
                  ConvertSuccess [TextObject] TextObject,
                  ConvertAttempt TextObject [TextObject],
                  ConvertSuccess [(a, TextObject)] TextObject,
                  ConvertAttempt TextObject [(a, TextObject)])
             => a -> Assertion
        test' a = do
            let dummy' = map (cs *** cs) dummy `asTypeOf` [(a, a)]
                dummy'' = toTextObject dummy'
            dummy'' @?= expected
            Just dummy' @=? fa (fromTextObject expected)
            let dummyO = map (cs *** toTextObject) dummy `asTypeOf`
                            [(a, undefined :: TextObject)]
            expected @=? toTextObject dummyO
            Just dummyO @=? fa (fromTextObject expected)
    test' (undefined :: String)
    test' (undefined :: Text)
    test' (undefined :: BS.ByteString)
    test' (undefined :: BL.ByteString)
#endif

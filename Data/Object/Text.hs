{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    ) where

import Data.Object
import Data.Text.Lazy (Text)
import Data.Attempt

import Data.Convertible.Text

import Data.Time.Calendar
import Data.Ratio (Ratio)

import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad ((<=<))

-- | 'Object's with keys and values of type 'LT.Text'.
type TextObject = Object Text Text
instance ToObject Text Text Text where
    toObject = Scalar
instance FromObject Text Text Text where
    fromObject = fromScalar

-- | 'toObject' specialized for 'TextObject's
toTextObject :: ToObject a Text Text => a -> TextObject
toTextObject = toObject

-- | 'fromObject' specialized for 'TextObject's
fromTextObject :: FromObject a Text Text => TextObject -> Attempt a
fromTextObject = fromObject

instance ToObject (Object [Char] [Char]) Text Text where
    toObject = mapKeysValues convertSuccess convertSuccess

instance ToObject Char Text Text where
    toObject c = Scalar $ convertSuccess [c]
    listToObject = Scalar . convertSuccess
instance ToObject Day Text Text where
    toObject = Scalar . convertSuccess
instance ToObject Int Text Text where
    toObject = Scalar . convertSuccess
instance ToObject (Ratio Integer) Text Text where
    toObject = Scalar . convertSuccess
instance ToObject Bool Text Text where
    toObject = Scalar . convertSuccess

newtype ExpectedCharException = ExpectedCharException String
    deriving (Show, Typeable)
instance Exception ExpectedCharException
instance FromObject Char Text Text where
    fromObject o = do
        x <- fromScalar o
        let y = convertSuccess x
        case y of
            [c] -> return c
            _ -> failure $ ExpectedCharException y
    listFromObject o = do
        x <- fromScalar o
        return $ convertSuccess x

instance FromObject Day Text Text where
    fromObject = convertAttempt <=< fromScalar
instance FromObject Int Text Text where
    fromObject = convertAttempt <=< fromScalar

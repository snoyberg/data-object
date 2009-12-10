{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------
--
-- Module        : Data.Object.String
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Objects with 'String's for keys and values.
---------------------------------------------------------

module Data.Object.String
    ( StringObject
    , toStringObject
    , fromStringObject
    ) where

import Data.Object
import Data.Object.Text (ExpectedCharException (..))
import Data.Attempt
import Control.Monad ((<=<))

import Data.Convertible.Text

import Data.Time.Calendar
import Data.Ratio (Ratio)

type StringObject = Object String String

-- | 'toObject' specialized for 'StringObject's
toStringObject :: ToObject a String String => a -> StringObject
toStringObject = toObject

-- | 'fromObject' specialized for 'StringObject's
fromStringObject :: FromObject a String String
                 => StringObject
                 -> Attempt a
fromStringObject = fromObject

instance ToObject Char [Char] [Char] where
    toObject = Scalar . return
    listToObject = Scalar
instance ToObject Day [Char] [Char] where
    toObject = Scalar . convertSuccess
instance ToObject Int [Char] [Char] where
    toObject = Scalar . convertSuccess
instance ToObject (Ratio Integer) [Char] [Char] where
    toObject = Scalar . convertSuccess
instance ToObject Bool [Char] [Char] where
    toObject = Scalar . convertSuccess

instance FromObject Char [Char] [Char] where
    fromObject o = do
        x <- fromScalar o
        case x of
            [c] -> return c
            _ -> failure $ ExpectedCharException x
    listFromObject = fromScalar
instance FromObject Day [Char] [Char] where
    fromObject = convertAttempt <=< fromScalar
instance FromObject Int [Char] [Char] where
    fromObject = convertAttempt <=< fromScalar
instance FromObject (Ratio Integer) [Char] [Char] where
    fromObject = convertAttempt <=< fromScalar
instance FromObject Bool [Char] [Char] where
    fromObject = convertAttempt <=< fromScalar

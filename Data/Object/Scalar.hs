{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Scalar
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
---------------------------------------------------------
module Data.Object.Scalar
    ( Scalar (..)
    , ScalarObject
    , toScalarObject
    , fromScalarObject
    ) where

import Data.ByteString.Lazy (ByteString, empty)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Object
import Data.Object.Raw
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Attempt
import Data.Convertible

data Scalar = Numeric   Rational
            | Text      Text
            | Binary    ByteString
            | Bool      Bool
            | Timestamp UTCTime
            | Null

type ScalarObject = Object String Scalar

instance ConvertAttempt Scalar Raw where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Scalar Raw where
    convertSuccess (Numeric n) = convertSuccess $ show n
    convertSuccess (Text t) = convertSuccess $ encodeUtf8 t
    convertSuccess (Binary b) = convertSuccess b
    convertSuccess (Bool True) = convertSuccess "true"
    convertSuccess (Bool False) = convertSuccess "false"
    -- this is W3 format for timestamps.
    convertSuccess (Timestamp t) =
        convertSuccess $ formatTime defaultTimeLocale "%FT%XZ" t
    convertSuccess Null = convertSuccess empty

-- | 'toObject' specialized for 'ScalarObject's
toScalarObject :: ToObject a String Scalar => a -> ScalarObject
toScalarObject = toObject

-- | 'fromObject' specialized for 'ScalarObject's
fromScalarObject :: FromObject a String Scalar
                 => ScalarObject
                 -> Attempt a
fromScalarObject = fromObject

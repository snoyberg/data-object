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
import Control.Monad.Attempt.Class

data Scalar = Numeric   Rational
            | Text      Text
            | Binary    ByteString
            | Bool      Bool
            | Timestamp UTCTime
            | Null

type ScalarObject = Object String Scalar

instance ToScalar Scalar Raw where
    toScalar (Numeric n) = toScalar $ show n
    toScalar (Text t) = toScalar $ encodeUtf8 t
    toScalar (Binary b) = toScalar b
    toScalar (Bool True) = toScalar "true"
    toScalar (Bool False) = toScalar "false"
    -- this is W3 format for timestamps.
    toScalar (Timestamp t) =
        toScalar $ formatTime defaultTimeLocale "%FT%XZ" t
    toScalar Null = toScalar empty

-- | 'toObject' specialized for 'ScalarObject's
toScalarObject :: ToObject a String Scalar => a -> ScalarObject
toScalarObject = toObject

-- | 'fromObject' specialized for 'ScalarObject's
fromScalarObject :: (FromObject a String Scalar, MonadAttempt m)
                 => ScalarObject
                 -> m a
fromScalarObject = fromObject

{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Object.Scalar
    ( Scalar (..)
    , ScalarObject
    ) where

import Data.ByteString.Lazy (ByteString, empty)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Object
import Data.Object.Raw
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

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

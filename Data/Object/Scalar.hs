{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , module Data.Object.Base
    ) where

import Data.ByteString.Lazy (ByteString, empty)
import Data.Time.Clock (UTCTime)
import Data.Object.Text
import Data.Object.Base
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Attempt
import Data.Convertible.Text

data Scalar = Numeric   Rational
            | Text      Text
            | Binary    ByteString
            | Bool      Bool
            | Timestamp UTCTime
            | Null

type ScalarObject = Object String Scalar

instance ConvertAttempt Scalar Text where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Scalar Text where
    convertSuccess (Numeric n) = convertSuccess $ show n
    convertSuccess (Text t) = t
    convertSuccess (Binary b) = convertSuccess b
    convertSuccess (Bool True) = convertSuccess "true"
    convertSuccess (Bool False) = convertSuccess "false"
    -- this is W3 format for timestamps.
    convertSuccess (Timestamp t) =
        convertSuccess $ formatTime defaultTimeLocale "%FT%XZ" t
    convertSuccess Null = convertSuccess empty

instance ConvertAttempt Text Scalar where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Text Scalar where
    convertSuccess = Text -- FIXME this should be more intelligent

instance ToObject (Object String Scalar) Text Text where
    toObject = mapKeysValues convertSuccess convertSuccess
instance FromObject (Object String Scalar) Text Text where
   fromObject = return . toObject

instance ToObject (Object Text Text) [Char] Scalar where
    toObject = mapKeysValues convertSuccess convertSuccess
instance FromObject (Object Text Text) [Char] Scalar where
   fromObject = return . toObject

-- | 'toObject' specialized for 'ScalarObject's
toScalarObject :: ToObject a String Scalar => a -> ScalarObject
toScalarObject = toObject

-- | 'fromObject' specialized for 'ScalarObject's
fromScalarObject :: FromObject a String Scalar
                 => ScalarObject
                 -> Attempt a
fromScalarObject = fromObject

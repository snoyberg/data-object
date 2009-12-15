{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
    , module Data.Object.Base
    ) where

import Data.Object.Base
import Data.Object.Text
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

instance ToObject String String String where
    toObject = Scalar
instance ToObject Day String String where
    toObject = Scalar . convertSuccess
instance ToObject Int String String where
    toObject = Scalar . convertSuccess
instance ToObject (Ratio Integer) String String where
    toObject = Scalar . convertSuccess
instance ToObject Bool String String where
    toObject = Scalar . convertSuccess

instance FromObject String String String where
    fromObject = convertAttempt <=< fromScalar
instance FromObject Day String String where
    fromObject = convertAttempt <=< fromScalar
instance FromObject Int String String where
    fromObject = convertAttempt <=< fromScalar
instance FromObject (Ratio Integer) String String where
    fromObject = convertAttempt <=< fromScalar
instance FromObject Bool String String where
    fromObject = convertAttempt <=< fromScalar

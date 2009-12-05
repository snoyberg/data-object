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

-- | This module makes all instances of 'ToObject' and 'FromObject' for
-- 'LT.Text' into instances for 'String'. It does so by allowing overlapping
-- instances: caveat emptor!
module Data.Object.String
    ( StringObject
    , toStringObject
    , fromStringObject
    ) where

import Data.Object
import Data.Attempt

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

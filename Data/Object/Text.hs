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
    ) where

import Data.Object
import Data.Text.Lazy (Text)
import Data.Attempt

import Data.Convertible
import Data.Convertible.Instances.String ()

import Data.Time.Calendar
import Data.Ratio (Ratio)

-- | 'Object's with keys and values of type 'LT.Text'.
type TextObject = Object Text Text

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

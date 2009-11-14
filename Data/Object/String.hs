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
import Data.Object.Text (fromTextObject)
import qualified Data.Text.Lazy as LT

type StringObject = Object String String

-- | 'toObject' specialized for 'StringObject's
toStringObject :: ToObject a String String => a -> StringObject
toStringObject = toObject

-- | 'fromObject' specialized for 'StringObject's
fromStringObject :: FromObject a String String
                 => StringObject
                 -> Attempt a
fromStringObject = fromObject

instance ToObject a LT.Text LT.Text => ToObject a String String where
    toObject = mapKeysValues LT.unpack LT.unpack . toObject

instance FromObject a LT.Text LT.Text => FromObject a String String where
    fromObject = fromTextObject . mapKeysValues LT.pack LT.pack

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , lookupStringObject
    ) where

import Data.Object

type StringObject = Object String String

-- | 'toObject' specialized for 'StringObject's
toStringObject :: ToObject a String String => a -> StringObject
toStringObject = toObject

-- | 'fomObject' specialized for 'StringObject's
fromStringObject :: (MonadFail m, FromObject a String String)
                 => StringObject
                 -> m a
fromStringObject = fromObject

-- | 'lookupObject' specialized for 'StringObject's
lookupStringObject :: ( MonadFail m
                      , ToScalar k String
                      , Show k
                      , FromObject v String String
                      )
                   => k
                   -> [(String, StringObject)]
                   -> m v
lookupStringObject = lookupObject

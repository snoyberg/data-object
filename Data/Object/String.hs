{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Attempt

import Data.Time.Calendar

type StringObject = Object String String

-- | 'toObject' specialized for 'StringObject's
toStringObject :: ConvertSuccess a StringObject => a -> StringObject
toStringObject = cs

-- | 'fromObject' specialized for 'StringObject's
fromStringObject :: ConvertAttempt StringObject a
                 => StringObject
                 -> Attempt a
fromStringObject = ca

$(deriveSuccessConvs ''String ''String
    [''String]
    [''String, ''Day, ''Int, ''Rational, ''Bool])

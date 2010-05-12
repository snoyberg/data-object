{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------
--
-- Module        : Data.Object
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- These objects show up in different places, eg JSON, Yaml.
-- By providing a representation in a separate repository,
-- other libraries can share a single representation of
-- these structures.
--
---------------------------------------------------------

-- | The core of this package is the 'Object' data type, which is used for
-- handling scalars, sequences and mappings in a nested manner. This
-- is the same structure used in JSON or Yaml data.
--
-- The 'Object' data type is polymorphic in its keys and values. Submodules
-- within this package provide more concrete datatypes, such as a 'String'
-- 'Object' and a specialized scalar type.
--
-- Besides the 'Object' data type, there are utility functions and type classes
-- for converting objects around. Care has been taken to avoid any overloaded
-- instances for these type classes.
module Data.Object
    ( -- * Object data type
      Object (..)
      -- ** Convenient type synonyms
    , StringObject
    , TextObject
      -- ** Scalar data type
    , Scalar (..)
    , ScalarObject
      -- * Basic mapping of keys and values
    , mapKeys
    , mapValues
    , mapKeysValues
    , mapKeysValuesA
    , mapKeysValuesM
      -- * Extracting underlying values
    , ObjectExtractError (..)
    , fromScalar
    , fromSequence
    , fromMapping
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad (ap)

import Prelude hiding (mapM, sequence)

import Data.Foldable hiding (concatMap, concat)
import Data.Traversable
import Data.Monoid

import Control.Exception (Exception)
import Data.Data (Data, Typeable)
import Control.Failure
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.ByteString (ByteString)

-- | Can represent nested values as scalars, sequences and mappings.  A
-- sequence is synonymous with a list, while a mapping is synonymous with a
-- list of pairs.
--
-- Note that instances of standard library type classes for this data type
-- leave the key untouched while altering the value. For example, the 'Functor'
-- instance defines 'fmap' to be synonymous with 'mapValues'.
data Object key val =
    Mapping [(key, Object key val)]
    | Sequence [Object key val]
    | Scalar val
    deriving (Show, Eq, Data, Typeable)

type StringObject = Object String String

-- | 'Object's with keys and values of strict 'Text'.
type TextObject = Object Text Text

data Scalar = Numeric   Rational
            | Text      Text
            | Binary    ByteString
            | Bool      Bool
            | Timestamp UTCTime
            | Null

type ScalarObject = Object String Scalar

instance Functor (Object key) where
    fmap = mapValues

instance Foldable (Object key) where
    foldMap f (Scalar v) = f v
    foldMap f (Sequence vs) = mconcat $ map (foldMap f) vs
    foldMap f (Mapping pairs) = mconcat $ map (foldMap f . snd) pairs

instance Traversable (Object key) where
    traverse f (Scalar v) = Scalar <$> f v
    traverse f (Sequence vs) = Sequence <$> traverse (traverse f) vs
    traverse f (Mapping pairs) =
      Mapping <$> traverse (traverse' (traverse f)) pairs

-- It would be nice if there were an "instance Traversable ((,) a)", but I
-- won't make an orphan instance simply for convenience. Instead:
traverse' :: Applicative f => (a -> f b) -> (x, a) -> f (x, b)
traverse' f (x, a) = (,) x <$> f a

joinObj :: Object key (Object key scalar) -> Object key scalar
joinObj (Scalar x)    = x
joinObj (Sequence xs) = Sequence (map joinObj xs)
joinObj (Mapping  xs) = Mapping  (map (second joinObj) xs)

instance Monad (Object key) where
    return = Scalar
    x >>= f = joinObj . fmap f $ x

instance Applicative (Object key) where
    pure  = Scalar
    (<*>) = ap

-- | Apply some conversion to the keys of an 'Object', leaving the values
-- unchanged.
mapKeys :: (keyIn -> keyOut) -> Object keyIn val -> Object keyOut val
mapKeys = flip mapKeysValues id

-- | Apply some conversion to the values of an 'Object', leaving the keys
-- unchanged. This is equivalent to 'fmap'.
mapValues :: (valIn -> valOut) -> Object key valIn -> Object key valOut
mapValues = mapKeysValues id

-- | Apply a conversion to both the keys and values of an 'Object'.
mapKeysValues :: (keyIn -> keyOut)
              -> (valIn -> valOut)
              -> Object keyIn valIn
              -> Object keyOut valOut
mapKeysValues _ fv (Scalar v) = Scalar $ fv v
mapKeysValues fk fv (Sequence os)= Sequence $ map (mapKeysValues fk fv) os
mapKeysValues fk fv (Mapping pairs) =
    Mapping $ map (fk *** mapKeysValues fk fv) pairs

-- | Apply an 'Applicative' conversion to both the keys and values of an
-- 'Object'.
mapKeysValuesA :: Applicative f
               => (keyIn -> f keyOut)
               -> (valIn -> f valOut)
               -> Object keyIn valIn
               -> f (Object keyOut valOut)
mapKeysValuesA _ fv (Scalar v) = Scalar <$> fv v
mapKeysValuesA fk fv (Sequence os) =
    Sequence <$> traverse (mapKeysValuesA fk fv) os
mapKeysValuesA fk fv (Mapping pairs) = Mapping <$>
    traverse (uncurry (liftA2 (,)) . (fk *** mapKeysValuesA fk fv)) pairs

-- | The same as 'mapKeysValuesA', but using a 'Monad' since some people are
-- more comfortable with 'Monad's and not all 'Monad's are 'Applicative'.
mapKeysValuesM :: Monad m
               => (keyIn -> m keyOut)
               -> (valIn -> m valOut)
               -> Object keyIn valIn
               -> m (Object keyOut valOut)
mapKeysValuesM fk fv =
    let fk' = WrapMonad . fk
        fv' = WrapMonad . fv
     in unwrapMonad . mapKeysValuesA fk' fv'

-- | An error value returned when an unexpected node is encountered, eg you
-- were expecting a 'Scalar' and found a 'Mapping'.
data ObjectExtractError =
    ExpectedScalar
    | ExpectedSequence
    | ExpectedMapping
    deriving (Typeable, Show)
instance Exception ObjectExtractError

-- | Extract a scalar from the input, failing if the input is a sequence or
-- mapping.
fromScalar :: Failure ObjectExtractError m => Object k v -> m v
fromScalar (Scalar s) = return s
fromScalar _ = failure ExpectedScalar

-- | Extract a sequence from the input, failing if the input is a scalar or
-- mapping.
fromSequence :: Failure ObjectExtractError m
             => Object k v
             -> m [Object k v]
fromSequence (Sequence s) = return s
fromSequence _ = failure ExpectedSequence

-- | Extract a mapping from the input, failing if the input is a scalar or
-- sequence.
fromMapping :: Failure ObjectExtractError m
            => Object k v
            -> m [(k, Object k v)]
fromMapping (Mapping m) = return m
fromMapping _ = failure ExpectedMapping

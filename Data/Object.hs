{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Data.Object
    ( Object (..)
    , mapKeys
    , mapValues
    , mapKeysValues
    , mapKeysValuesM
    , MonadFail
    , getScalar
    , getSequence
    , getMapping
    , ToScalar (..)
    , FromScalar (..)
    , ToObject (..)
    , FromObject (..)
    , Assoc (..) -- FIXME consider removing this again
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad (liftM2, ap, (<=<))

import Prelude hiding (mapM, sequence)

import Data.Foldable
import Data.Traversable
import Data.Monoid

class (Functor m, Applicative m, Monad m) => MonadFail m where

instance MonadFail IO where
instance MonadFail Maybe where
instance MonadFail [] where

data Object key val =
    Mapping [(key, Object key val)]
    | Sequence [Object key val]
    | Scalar val
    deriving (Show, Eq)

mapKeys :: (key1 -> key2) -> Object key1 val -> Object key2 val
mapKeys = flip mapKeysValues id

-- | This is equivalent to 'fmap'.
mapValues :: (val1 -> val2) -> Object key val1 -> Object key val2
mapValues = mapKeysValues id

mapKeysValues :: (key1 -> key2)
              -> (val1 -> val2)
              -> Object key1 val1
              -> Object key2 val2
mapKeysValues _ fv (Scalar v) = Scalar $ fv v
mapKeysValues fk fv (Sequence os)= Sequence $ map (mapKeysValues fk fv) os
mapKeysValues fk fv (Mapping pairs) =
    Mapping $ map (fk *** mapKeysValues fk fv) pairs

mapKeysValuesM :: MonadFail m
               => (key1 -> m key2)
               -> (val1 -> m val2)
               -> Object key1 val1
               -> m (Object key2 val2)
mapKeysValuesM _ fv (Scalar v) = Scalar <$> fv v
mapKeysValuesM fk fv (Sequence os) =
    Sequence <$> mapM (mapKeysValuesM fk fv) os
mapKeysValuesM fk fv (Mapping pairs) = Mapping <$>
    mapM (uncurry (liftM2 (,)) . (fk *** mapKeysValuesM fk fv)) pairs

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

getScalar :: MonadFail m => Object k v -> m v
getScalar (Scalar s) = return s
getScalar _ = fail "Attempt to extract a scalar from non-scalar"

getSequence :: MonadFail m => Object k v -> m [Object k v]
getSequence (Sequence s) = return s
getSequence _ = fail "Attempt to extract a sequence from non-sequence"

getMapping :: MonadFail m => Object k v -> m [(k, Object k v)]
getMapping (Mapping m) = return m
getMapping _ = fail "Attempt to extract a mapping from non-mapping"

class ToScalar x y where
    toScalar :: x -> y
class FromScalar x y where
    fromScalar :: MonadFail m => y -> m x
class ToObject a k v where
    toObject :: a -> Object k v

    listToObject :: [a] -> Object k v
    listToObject = Sequence . map toObject

    -- FIXME is this actually necesary?
    mapToObject :: ToScalar k' k => [(k', a)] -> Object k v
    mapToObject = Mapping . map (toScalar *** toObject)

class FromObject a k v where
    fromObject :: MonadFail m => Object k v -> m a

    listFromObject :: MonadFail m => Object k v -> m [a]
    listFromObject = mapM fromObject <=< getSequence

    -- FIXME is this actually necesary?
    mapFromObject :: (FromScalar k' k, MonadFail m)
                  => Object k v
                  -> m [(k', a)]
    mapFromObject =
        mapM (runKleisli (Kleisli fromScalar *** Kleisli fromObject))
         <=< getMapping

-- Converting between different types of Objects
instance (ToScalar k k', ToScalar v v') => ToObject (Object k v) k' v' where
    toObject = mapKeysValues toScalar toScalar

instance (FromScalar k k', FromScalar v v')
  => FromObject (Object k v) k' v' where
    fromObject = mapKeysValuesM fromScalar fromScalar

{- FIXME causes too much overlapping
-- Special To/FromScalar => To/FromObject instances
instance ToScalar x y => ToObject x k y where
    toObject = Scalar . toScalar
instance FromScalar x y => FromObject x k y where
    fromObject = fromScalar <=< getScalar
-}

-- Sequence
instance ToObject a k v => ToObject [a] k v where
    toObject = listToObject
instance FromObject a k v => FromObject [a] k v where
    fromObject = listFromObject

-- Mapping
newtype Assoc k v = Assoc { unAssoc :: [(k, v)] }
    deriving (Eq, Show)
instance (ToObject a k v, ToScalar k' k)
  => ToObject (Assoc k' a) k v where
    toObject = mapToObject . unAssoc
instance (FromObject a k v, FromScalar k' k)
  => FromObject (Assoc k' a) k v where
    fromObject = fmap Assoc . mapFromObject

instance (ToScalar k k', ToObject v k' v') => ToObject (k, v) k' v' where
    toObject = listToObject . return
    listToObject = Mapping . map (toScalar *** toObject)
instance (FromScalar k k', FromObject v k' v') => FromObject (k, v) k' v' where
    fromObject o = do
        ms <- listFromObject o
        case ms of
            [m] -> return m
            _ -> fail "fromObject of pair requires mapping of size 1"
    listFromObject =
        mapM (runKleisli (Kleisli fromScalar *** Kleisli fromObject))
        <=< getMapping

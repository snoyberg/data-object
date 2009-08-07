{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
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
    , FromObject (..)
    , ToObject (..)
    , oLookup
    ) where

import qualified Data.ByteString as B
import Data.ByteString.Class
import Control.Arrow

data Object =
    Mapping [(B.ByteString, Object)]
    | Sequence [Object]
    | Scalar B.ByteString
    deriving (Show)

class ToObject a where
    toObject :: a -> Object

class FromObject a where
    fromObject :: Monad m => Object -> m a

bsFromObject :: (Monad m, StrictByteString bs) => Object -> m bs
bsFromObject (Scalar bs) = return $ fromStrictByteString bs
bsFromObject _ = fail "Attempt to extract a scalar from non-scalar"

instance ToObject String where
    toObject = Scalar . toStrictByteString

instance FromObject String where
    fromObject = bsFromObject

instance ToObject B.ByteString where
    toObject = Scalar

instance FromObject B.ByteString where
    fromObject = bsFromObject

instance ToObject o => ToObject [o] where
    toObject = Sequence . map toObject

instance FromObject o => FromObject [o] where
    fromObject (Sequence os) = mapM fromObject os
    fromObject _ = fail "Attempt to extract a sequence from non-sequence"

instance (StrictByteString bs, ToObject o) => ToObject [(bs, o)] where
    toObject = Mapping . map (toStrictByteString *** toObject)

instance (StrictByteString bs, FromObject o) => FromObject [(bs, o)] where
    fromObject (Mapping pairs) =
        mapM (liftSnd . (fromStrictByteString *** fromObject)) pairs
    fromObject _ = fail "Attempt to extract a mapping from non-mapping"

instance ToObject Object where
    toObject = id

instance FromObject Object where
    fromObject = return

liftSnd :: Monad m => (a, m b) -> m (a, b)
liftSnd (a, b) = b >>= \b' -> return (a, b')

oLookup :: (Monad m, Eq a, Show a, FromObject b)
        => a -- ^ key
        -> [(a, Object)]
        -> m b
oLookup key pairs =
    case lookup key pairs of
        Nothing -> fail $ "Key not found: " ++ show key
        Just x -> fromObject x

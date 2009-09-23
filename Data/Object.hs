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
    ( Object
    , GenObject (..)
    , FromObject (..)
    , ToObject (..)
    , FromScalar (..)
    , ToScalar (..)
    , oLookup
    ) where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Class
import Control.Arrow
import Data.Time.Calendar

data GenObject key val =
    Mapping [(key, GenObject key val)]
    | Sequence [GenObject key val]
    | Scalar val
    deriving (Show)

type Object = GenObject B.ByteString B.ByteString

class ToObject a where
    toObject :: a -> Object
class FromObject a where
    fromObject :: Monad m => Object -> m a

class ToObject a => ToScalar a where
    toScalar :: a -> B.ByteString
class FromObject a => FromScalar a where
    fromScalar :: Monad m => B.ByteString -> m a

bsFromObject :: Monad m => Object -> m B.ByteString
bsFromObject (Scalar s) = return s
bsFromObject _ = fail "Attempt to extract a scalar from non-scalar"

instance ToScalar B.ByteString where
    toScalar = id
instance FromScalar B.ByteString where
    fromScalar = return
instance ToObject B.ByteString where
    toObject = Scalar
instance FromObject B.ByteString where
    fromObject = bsFromObject

instance ToScalar String where
    toScalar = toLazyByteString
instance FromScalar String where
    fromScalar = return . fromLazyByteString
instance ToObject String where
    toObject = Scalar . toScalar
instance FromObject String where
    fromObject o = fromObject o >>= fromScalar

instance ToObject o => ToObject [o] where
    toObject = Sequence . map toObject

instance FromObject o => FromObject [o] where
    fromObject (Sequence os) = mapM fromObject os
    fromObject _ = fail "Attempt to extract a sequence from non-sequence"

instance (ToScalar bs, ToObject o) => ToObject [(bs, o)] where
    toObject = Mapping . map (toScalar *** toObject)

instance (FromScalar bs, FromObject o) => FromObject [(bs, o)] where
    fromObject (Mapping pairs) =
        mapM (liftPair . (fromScalar *** fromObject)) pairs
    fromObject _ = fail "Attempt to extract a mapping from non-mapping"

instance ToObject Object where
    toObject = id

instance FromObject Object where
    fromObject = return

liftPair :: Monad m => (m a, m b) -> m (a, b)
liftPair (a, b) = do
    a' <- a
    b' <- b
    return (a', b')

oLookup :: (Monad m, Eq a, Show a, FromObject b)
        => a -- ^ key
        -> [(a, Object)]
        -> m b
oLookup key pairs =
    case lookup key pairs of
        Nothing -> fail $ "Key not found: " ++ show key
        Just x -> fromObject x

-- helper
readM :: (Read r, Monad m) => String -> m r
readM s = case reads s of
            ((x, _):_) -> return x
            _ -> fail $ "Unable to read: " ++ s
-- instances

instance ToScalar Day where
    toScalar = toLazyByteString . show
instance ToObject Day where
    toObject = toObject . toScalar
instance FromScalar Day where
    fromScalar bs = do
        let s = fromLazyByteString bs
        if length s /= 10
            then fail ("Invalid day: " ++ s)
            else do
                y <- readM $ take 4 s
                m <- readM $ take 2 $ drop 5 s
                d <- readM $ take 2 $ drop 8 s
                return $ fromGregorian y m d
instance FromObject Day where
    fromObject o = fromObject o >>= fromScalar

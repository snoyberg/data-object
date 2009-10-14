{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Raw
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Objects with bytestrings for keys and values. This also includes some nice
-- utilities for converting to/from these.
--
-- This is especially useful for serializing to/from files like JSON and Yaml.
---------------------------------------------------------
module Data.Object.Raw
    ( RawObject
    , FromRawObject (..)
    , ToRawObject (..)
    , FromRaw (..)
    , ToRaw (..)
    , oLookup
    , module Data.Object
    ) where

import Data.Object
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ByteString.Class
import Data.Time.Calendar
import Safe (readMay)
import Control.Monad (liftM2)
import Control.Arrow

type Raw = B.ByteString

type RawObject = Object Raw Raw

class ToRawObject a where
    toRawObject :: a -> RawObject
class FromRawObject a where
    fromRawObject :: MonadFail m => RawObject -> m a

class ToRawObject a => ToRaw a where
    toRaw :: a -> B.ByteString
class FromRawObject a => FromRaw a where
    fromRaw :: MonadFail m => B.ByteString -> m a

rawFromRawObject :: MonadFail m => RawObject -> m Raw
rawFromRawObject (Scalar s) = return s
rawFromRawObject _ = fail "Attempt to extract a scalar from non-scalar"

instance ToRaw Raw where
    toRaw = id
instance FromRaw Raw where
    fromRaw = return
instance ToRawObject Raw where
    toRawObject = Scalar
instance FromRawObject Raw where
    fromRawObject = rawFromRawObject

instance ToRaw BS.ByteString where
    toRaw = toLazyByteString
instance FromRaw BS.ByteString where
    fromRaw = return . fromLazyByteString
instance ToRawObject BS.ByteString where
    toRawObject = Scalar . toRaw
instance FromRawObject BS.ByteString where
    fromRawObject o = fromRawObject o >>= fromRaw

instance ToRaw String where
    toRaw = toLazyByteString
instance FromRaw String where
    fromRaw = return . fromLazyByteString
instance ToRawObject String where
    toRawObject = Scalar . toRaw
instance FromRawObject String where
    fromRawObject o = fromRawObject o >>= fromRaw

instance ToRawObject o => ToRawObject [o] where
    toRawObject = Sequence . map toRawObject
instance FromRawObject o => FromRawObject [o] where
    fromRawObject (Sequence os) = mapM fromRawObject os
    fromRawObject _ = fail "Attempt to extract a sequence from non-sequence"

instance (ToRaw bs, ToRawObject o) => ToRawObject [(bs, o)] where
    toRawObject = Mapping . map (toRaw *** toRawObject)
instance (FromRaw bs, FromRawObject o) => FromRawObject [(bs, o)] where
    fromRawObject (Mapping pairs) =
        mapM (uncurry (liftM2 (,)) . (fromRaw *** fromRawObject)) pairs
    fromRawObject _ = fail "Attempt to extract a mapping from non-mapping"

instance ToRawObject RawObject where
    toRawObject = id
instance FromRawObject RawObject where
    fromRawObject = return

instance (ToRaw key, ToRaw value) => ToRawObject (Object key value) where
    toRawObject = mapKeysValues toRaw toRaw
instance (FromRaw key, FromRaw value) => FromRawObject (Object key value) where
    fromRawObject = mapKeysValuesM fromRaw fromRaw

oLookup :: (MonadFail m, Eq a, Show a, FromRawObject b)
        => a -- ^ key
        -> [(a, RawObject)]
        -> m b
oLookup key pairs =
    case lookup key pairs of
        Nothing -> fail $ "Key not found: " ++ show key
        Just x -> fromRawObject x

-- instances

instance ToRaw Day where
    toRaw = toLazyByteString . show
instance ToRawObject Day where
    toRawObject = toRawObject . toRaw
instance FromRaw Day where
    fromRaw bs = do
        let s = fromLazyByteString bs
        if length s /= 10
            then fail ("Invalid day: " ++ s)
            else do
                let x = do
                    y' <- readMay $ take 4 s
                    m' <- readMay $ take 2 $ drop 5 s
                    d' <- readMay $ take 2 $ drop 8 s
                    return (y', m', d')
                case x of
                    Just (y, m, d) -> return $ fromGregorian y m d
                    Nothing -> fail $ "Invalid day: " ++ s
instance FromRawObject Day where
    fromRawObject o = fromRawObject o >>= fromRaw

instance ToRaw Bool where
    toRaw b = toRaw $ if b then "true" else "false"
instance ToRawObject Bool where
    toRawObject = toRawObject . toRaw
instance FromRaw Bool where
    fromRaw bs =
        case fromLazyByteString bs of
            "true" -> return True
            "false" -> return False
            x -> fail $ "Invalid bool value: " ++ x
instance FromRawObject Bool where
    fromRawObject o = fromRawObject o >>= fromRaw

instance ToRaw Int where
    toRaw = toRaw . show
instance ToRawObject Int where
    toRawObject = toRawObject . toRaw
instance FromRaw Int where
    fromRaw bs =
        case readMay $ fromLazyByteString bs of
            Nothing -> fail $ "Invalid integer: " ++ fromLazyByteString bs
            Just i -> return i
instance FromRawObject Int where
    fromRawObject o = fromRawObject o >>= fromRaw

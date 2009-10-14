{-# LANGUAGE FlexibleInstances #-}
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
import Control.Monad ((<=<))
import Control.Arrow

newtype Raw = Raw { unRaw :: B.ByteString }

type RawObject = Object Raw Raw

class ToRaw a where
    toRaw :: a -> Raw
class FromRaw a where
    fromRaw :: MonadFail m => Raw -> m a

class ToRawObject a where
    toRawObject :: a -> RawObject

    listToRawObject :: [a] -> RawObject
    listToRawObject = Sequence . map toRawObject

    mapToRawObject :: ToRaw k => [(k, a)] -> RawObject
    mapToRawObject = Mapping . map (toRaw *** toRawObject)

class FromRawObject a where
    fromRawObject :: MonadFail m => RawObject -> m a

    listFromRawObject :: MonadFail m => RawObject -> m [a]
    listFromRawObject = mapM fromRawObject <=< getSequence

    mapFromRawObject :: (FromRaw k, MonadFail m) => RawObject -> m [(k, a)]
    mapFromRawObject =
        mapM (runKleisli (Kleisli fromRaw *** Kleisli fromRawObject))
         <=< getMapping

oLookup :: (MonadFail m, Eq a, Show a, FromRawObject b)
        => a -- ^ key
        -> [(a, RawObject)]
        -> m b
oLookup key pairs =
    case lookup key pairs of
        Nothing -> fail $ "Key not found: " ++ show key
        Just x -> fromRawObject x

-- special list instance
instance ToRawObject a => ToRawObject [a] where
    toRawObject = listToRawObject
instance FromRawObject a => FromRawObject [a] where
    fromRawObject = listFromRawObject

-- special map instance
instance (ToRaw k, ToRawObject v) => ToRawObject [(k, v)] where
    toRawObject = mapToRawObject
instance (FromRaw k, FromRawObject v) => FromRawObject [(k, v)] where
    fromRawObject = mapFromRawObject

-- Raw instances
instance ToRaw Raw where
    toRaw = id
instance FromRaw Raw where
    fromRaw = return
instance ToRawObject Raw where
    toRawObject = Scalar
instance FromRawObject Raw where
    fromRawObject = getScalar

-- lazy bytestrings
instance ToRaw B.ByteString where
    toRaw = Raw
instance FromRaw B.ByteString where
    fromRaw = return . unRaw
instance ToRawObject B.ByteString where
    toRawObject = Scalar . Raw
instance FromRawObject B.ByteString where
    fromRawObject = fmap unRaw . getScalar

-- strict bytestrings
instance ToRaw BS.ByteString where
    toRaw = Raw . toLazyByteString
instance FromRaw BS.ByteString where
    fromRaw = return . fromLazyByteString . unRaw
instance ToRawObject BS.ByteString where
    toRawObject = Scalar . toRaw
instance FromRawObject BS.ByteString where
    fromRawObject = fmap (fromLazyByteString . unRaw) . getScalar

-- Chars (and thereby strings)
-- Extra complication since we're avoiding overlapping instances.
class ListToRaw a where
    listToRaw :: [a] -> Raw
instance ListToRaw a => ToRaw [a] where
    toRaw = listToRaw
instance ListToRaw Char where
    listToRaw = Raw . toLazyByteString

class ListFromRaw a where
    listFromRaw :: MonadFail m => Raw -> m [a]
instance ListFromRaw a => FromRaw [a] where
    fromRaw = listFromRaw
instance ListFromRaw Char where
    listFromRaw = return . fromLazyByteString . unRaw

instance ToRawObject Char where
    toRawObject c = Scalar $ Raw $ toLazyByteString $ [c]
    listToRawObject = Scalar . Raw . toLazyByteString
instance FromRawObject Char where
    fromRawObject = helper . fromLazyByteString . unRaw <=< getScalar where
        helper :: MonadFail m => String -> m Char
        helper [x] = return x
        helper x = fail $ "Excepting a single character, received: " ++ x
    listFromRawObject = fmap (fromLazyByteString . unRaw) . getScalar

-- Objects
instance (ToRaw k, ToRaw v) => ToRawObject (Object k v) where
    toRawObject = mapKeysValues toRaw toRaw
instance (FromRaw k, FromRaw v) => FromRawObject (Object k v) where
    fromRawObject = mapKeysValuesM fromRaw fromRaw

-- Day
instance ToRaw Day where
    toRaw = Raw . toLazyByteString . show
instance ToRawObject Day where
    toRawObject = Scalar . toRaw
instance FromRaw Day where
    fromRaw (Raw bs) = do
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
    fromRawObject = fromRaw <=< getScalar

-- Bool
instance ToRaw Bool where
    toRaw b = Raw $ toLazyByteString $ if b then "true" else "false"
instance ToRawObject Bool where
    toRawObject = Scalar . toRaw
instance FromRaw Bool where
    fromRaw (Raw bs) =
        case fromLazyByteString bs of -- FIXME add other values here
                                      -- maybe be case insensitive
            "true" -> return True
            "false" -> return False
            x -> fail $ "Invalid bool value: " ++ x
instance FromRawObject Bool where
    fromRawObject = fromRaw <=< getScalar

-- Int
instance ToRaw Int where
    toRaw = Raw . toLazyByteString . show
instance ToRawObject Int where
    toRawObject = Scalar . toRaw
instance FromRaw Int where
    fromRaw (Raw bs) =
        case readMay $ fromLazyByteString bs of
            Nothing -> fail $ "Invalid integer: " ++ fromLazyByteString bs
            Just i -> return i
instance FromRawObject Int where
    fromRawObject = fromRaw <=< getScalar

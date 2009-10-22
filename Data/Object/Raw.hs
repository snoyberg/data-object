{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
-- Objects with bytestrings for keys and values. This is especially useful for
-- serializing to/from files like JSON and Yaml.
---------------------------------------------------------
module Data.Object.Raw
    ( Raw (..)
    , RawObject
    , toRawObject
    , fromRawObject
    ) where

import Data.Object
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.ByteString.Class
import Data.Time.Calendar
import Safe (readMay)
import Control.Monad ((<=<))
import Data.Ratio (Ratio)
import Data.Attempt
import Data.Generics

-- | A thin wrapper around a lazy bytestring.
newtype Raw = Raw { unRaw :: B.ByteString }
    deriving (Eq, Typeable, Data)
instance Show Raw where
    show = show . unRaw

-- | 'Object's with keys and values of type 'Raw'.
type RawObject = Object Raw Raw

-- lazy bytestrings
instance ToScalar B.ByteString Raw where
    toScalar = Raw
instance ToScalar Raw B.ByteString where
    toScalar = unRaw
instance FromScalar B.ByteString Raw where
    fromScalar = return . toScalar
instance FromScalar Raw B.ByteString where
    fromScalar = return . toScalar
instance ToObject B.ByteString a Raw where
    toObject = scalarToObject
instance FromObject B.ByteString a Raw where
    fromObject = scalarFromObject

-- strict bytestrings
instance ToScalar BS.ByteString Raw where
    toScalar = Raw . toLazyByteString
instance ToScalar Raw BS.ByteString where
    toScalar = fromLazyByteString . unRaw
instance FromScalar BS.ByteString Raw where
    fromScalar = return . toScalar
instance FromScalar Raw BS.ByteString where
    fromScalar = return . toScalar
instance ToObject BS.ByteString a Raw where
    toObject = scalarToObject
instance FromObject BS.ByteString a Raw where
    fromObject = scalarFromObject

-- Chars (and thereby strings)
-- Extra complication since we're avoiding overlapping instances.
class ListToRaw a where
    listToRaw :: [a] -> Raw
instance ListToRaw a => ToScalar [a] Raw where
    toScalar = listToRaw
instance ListToRaw Char where
    listToRaw = Raw . toLazyByteString

class ListFromRaw a where
    listFromRaw :: MonadAttempt m => Raw -> m [a]
instance ListFromRaw a => FromScalar [a] Raw where
    fromScalar = listFromRaw
instance ListFromRaw Char where
    listFromRaw = return . fromLazyByteString . unRaw

instance ToObject Char Raw Raw where
    toObject c = Scalar $ Raw $ toLazyByteString [c]
    listToObject = Scalar . Raw . toLazyByteString
instance FromObject Char Raw Raw where
    fromObject = helper . fromLazyByteString . unRaw <=< getScalar where
        helper :: MonadAttempt m => String -> m Char
        helper [x] = return x
        helper x = fail $ "Excepting a single character, received: " ++ x
    listFromObject = fmap (fromLazyByteString . unRaw) . getScalar

-- Day
instance ToScalar Day Raw where
    toScalar = Raw . toLazyByteString . show
instance ToObject Day k Raw where
    toObject = scalarToObject
instance FromScalar Day Raw where
    fromScalar (Raw bs) = do
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
instance FromObject Day k Raw where
    fromObject = scalarFromObject

-- Bool
instance ToScalar Bool Raw where
    toScalar b = Raw $ toLazyByteString $ if b then "true" else "false"
instance ToObject Bool k Raw where
    toObject = scalarToObject
instance FromScalar Bool Raw where
    fromScalar (Raw bs) =
        case fromLazyByteString bs of
            -- list comes from http://yaml.org/type/bool.html
            "y" -> return True
            "Y" -> return True
            "yes" -> return True
            "Yes" -> return True
            "YES" -> return True
            "true" -> return True
            "True" -> return True
            "TRUE" -> return True
            "on" -> return True
            "On" -> return True
            "ON" -> return True

            "n" -> return False
            "N" -> return False
            "no" -> return False
            "No" -> return False
            "NO" -> return False
            "false" -> return False
            "False" -> return False
            "FALSE" -> return False
            "off" -> return False
            "Off" -> return False
            "OFF" -> return False

            x -> fail $ "Invalid bool value: " ++ x
instance FromObject Bool k Raw where
    fromObject = scalarFromObject

-- Int
instance ToScalar Int Raw where
    toScalar = Raw . toLazyByteString . show
instance ToObject Int k Raw where
    toObject = scalarToObject
instance FromScalar Int Raw where
    fromScalar (Raw bs) =
        case readMay $ fromLazyByteString bs of
            Nothing -> fail $ "Invalid integer: " ++ fromLazyByteString bs
            Just i -> return i
instance FromObject Int k Raw where
    fromObject = scalarFromObject

-- Rational
instance ToScalar (Ratio Integer) Raw where
    toScalar = Raw . toLazyByteString . show
instance ToObject (Ratio Integer) k Raw where
    toObject = scalarToObject
instance FromScalar (Ratio Integer) Raw where
    fromScalar (Raw bs) =
        case readMay $ fromLazyByteString bs of
            Nothing -> fail $ "Invalid rational: " ++ fromLazyByteString bs
            Just i -> return i
instance FromObject (Ratio Integer) k Raw where
    fromObject = scalarFromObject

-- | 'toObject' specialized for 'RawObject's
toRawObject :: ToObject a Raw Raw => a -> RawObject
toRawObject = toObject

-- | 'fromObject' specialized for 'RawObject's
fromRawObject :: FromObject a Raw Raw => RawObject -> Attempt a
fromRawObject = fromObject

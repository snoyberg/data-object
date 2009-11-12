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
import Control.Exception (Exception)
import Data.Convertible

-- | A thin wrapper around a lazy bytestring.
newtype Raw = Raw { unRaw :: B.ByteString }
    deriving (Eq, Typeable, Data)
instance Show Raw where
    show = show . unRaw

-- | 'Object's with keys and values of type 'Raw'.
type RawObject = Object Raw Raw

-- lazy bytestrings
instance ConvertSuccess B.ByteString Raw where
    convertSuccess = Raw
instance ConvertSuccess Raw B.ByteString where
    convertSuccess = unRaw
instance ConvertAttempt B.ByteString Raw where
    convertAttempt = return . convertSuccess
instance ConvertAttempt Raw B.ByteString where
    convertAttempt = return . convertSuccess
instance ToObject B.ByteString a Raw where
    toObject = scalarToObject
instance FromObject B.ByteString a Raw where
    fromObject = scalarFromObject

-- strict bytestrings
instance ConvertSuccess BS.ByteString Raw where
    convertSuccess = Raw . toLazyByteString
instance ConvertSuccess Raw BS.ByteString where
    convertSuccess = fromLazyByteString . unRaw
instance ConvertAttempt BS.ByteString Raw where
    convertAttempt = return . convertSuccess
instance ConvertAttempt Raw BS.ByteString where
    convertAttempt = return . convertSuccess
instance ToObject BS.ByteString a Raw where
    toObject = scalarToObject
instance FromObject BS.ByteString a Raw where
    fromObject = scalarFromObject

-- Chars (and thereby strings)
-- Extra complication since we're avoiding overlapping instances.
class ListToRaw a where
    listToRaw :: [a] -> Raw
instance ListToRaw a => ConvertAttempt [a] Raw where
    convertAttempt = return . convertSuccess
instance ListToRaw a => ConvertSuccess [a] Raw where
    convertSuccess = listToRaw
instance ListToRaw Char where
    listToRaw = Raw . toLazyByteString

class ListFromRaw a where
    listFromRaw :: MonadFailure ExpectedSingleCharacter m => Raw -> m [a] -- FIXME rather ugly
instance ListFromRaw a => ConvertAttempt Raw [a] where
    convertAttempt = listFromRaw
instance ListFromRaw Char where
    listFromRaw = return . fromLazyByteString . unRaw

data ExpectedSingleCharacter = ExpectedSingleCharacter String
    deriving (Show, Typeable)
instance Exception ExpectedSingleCharacter

instance ToObject Char Raw Raw where
    toObject c = Scalar $ Raw $ toLazyByteString [c]
    listToObject = Scalar . Raw . toLazyByteString
instance FromObject Char Raw Raw where
    fromObject = helper . fromLazyByteString . unRaw <=< getScalar where
        helper [x] = return x
        helper x = failure $ ExpectedSingleCharacter x
    listFromObject = fmap (fromLazyByteString . unRaw) . getScalar

-- Day
instance ConvertSuccess Day Raw where
    convertSuccess = Raw . toLazyByteString . show
instance ConvertAttempt Day Raw where
    convertAttempt = return . convertSuccess
instance ToObject Day k Raw where
    toObject = scalarToObject
instance ConvertAttempt Raw Day where
    convertAttempt (Raw bs) = do
        let s = fromLazyByteString bs
        if length s /= 10
            then failureString ("Invalid day: " ++ s)
            else do
                let x = do
                    y' <- readMay $ take 4 s
                    m' <- readMay $ take 2 $ drop 5 s
                    d' <- readMay $ take 2 $ drop 8 s
                    return (y', m', d')
                case x of
                    Just (y, m, d) -> return $ fromGregorian y m d
                    Nothing -> failureString $ "Invalid day: " ++ s
instance FromObject Day k Raw where
    fromObject = scalarFromObject

-- Bool
instance ConvertAttempt Bool Raw where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Bool Raw where
    convertSuccess b = Raw $ toLazyByteString $ if b then "true" else "false"
instance ToObject Bool k Raw where
    toObject = scalarToObject
instance ConvertAttempt Raw Bool where
    convertAttempt (Raw bs) =
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

            x -> failureString $ "Invalid bool value: " ++ x
instance FromObject Bool k Raw where
    fromObject = scalarFromObject

-- Int
instance ConvertSuccess Int Raw where
    convertSuccess = Raw . toLazyByteString . show
instance ConvertAttempt Int Raw where
    convertAttempt = return . convertSuccess
instance ToObject Int k Raw where
    toObject = scalarToObject
instance ConvertAttempt Raw Int where
    convertAttempt (Raw bs) =
        case readMay $ fromLazyByteString bs of
            Nothing ->
                failureString $ "Invalid integer: " ++ fromLazyByteString bs
            Just i -> return i
instance FromObject Int k Raw where
    fromObject = scalarFromObject

-- Rational
instance ConvertSuccess (Ratio Integer) Raw where
    convertSuccess = Raw . toLazyByteString . show
instance ConvertAttempt (Ratio Integer) Raw where
    convertAttempt = return . convertSuccess
instance ToObject (Ratio Integer) k Raw where
    toObject = scalarToObject
instance ConvertAttempt Raw (Ratio Integer) where
    convertAttempt (Raw bs) =
        case readMay $ fromLazyByteString bs of
            Nothing ->
                failureString $ "Invalid rational: " ++ fromLazyByteString bs
            Just i -> return i
instance FromObject (Ratio Integer) k Raw where
    fromObject = scalarFromObject

-- | 'toObject' specialized for 'RawObject's
toRawObject :: ToObject a Raw Raw => a -> RawObject
toRawObject = toObject

-- | 'fromObject' specialized for 'RawObject's
fromRawObject :: FromObject a Raw Raw => RawObject -> Attempt a
fromRawObject = fromObject

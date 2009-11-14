{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Text
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
---------------------------------------------------------

-- FIXME should the convertible instances be moved to convertible?

-- | Keys and values are lazy 'LT.Text's.
module Data.Object.Text
    ( TextObject
    , toTextObject
    , fromTextObject
    ) where

import Data.Object
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString.Lazy as BL
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
import qualified Safe.Failure as SF

-- | 'Object's with keys and values of type 'LT.Text'.
type TextObject = Object LT.Text LT.Text

-- lazy bytestrings
instance ConvertSuccess BL.ByteString LT.Text where
    convertSuccess = LTE.decodeUtf8
instance ConvertAttempt BL.ByteString LT.Text where
    convertAttempt = return . convertSuccess

instance ConvertSuccess LT.Text BL.ByteString where
    convertSuccess = LTE.encodeUtf8
instance ConvertAttempt LT.Text BL.ByteString where
    convertAttempt = return . convertSuccess

instance ToObject BL.ByteString a LT.Text where
    toObject = scalarToObject
instance FromObject BL.ByteString a LT.Text where
    fromObject = scalarFromObject

-- strict bytestrings
instance ConvertSuccess BS.ByteString LT.Text where
    convertSuccess = LTE.decodeUtf8 . toLazyByteString
instance ConvertSuccess LT.Text BS.ByteString where
    convertSuccess = fromLazyByteString . LTE.encodeUtf8
instance ConvertAttempt BS.ByteString LT.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt LT.Text BS.ByteString where
    convertAttempt = return . convertSuccess
instance ToObject BS.ByteString a LT.Text where
    toObject = scalarToObject
instance FromObject BS.ByteString a LT.Text where
    fromObject = scalarFromObject

-- Chars (and thereby strings)
-- Extra complication since we're avoiding overlapping instances.
class ListToText a where
    listToText :: [a] -> LT.Text
instance ListToText a => ConvertAttempt [a] LT.Text where
    convertAttempt = return . convertSuccess
instance ListToText a => ConvertSuccess [a] LT.Text where
    convertSuccess = listToText
instance ListToText Char where
    listToText = LT.pack

class ListFromText a where
    listFromText :: LT.Text -> [a]
instance ListFromText a => ConvertSuccess LT.Text [a] where
    convertSuccess = listFromText
instance ListFromText a => ConvertAttempt LT.Text [a] where
    convertAttempt = return . convertSuccess
instance ListFromText Char where
    listFromText = LT.unpack

data ExpectedSingleCharacter = ExpectedSingleCharacter String
    deriving (Show, Typeable)
instance Exception ExpectedSingleCharacter

instance ToObject Char LT.Text LT.Text where
    toObject c = Scalar $ LT.pack [c]
    listToObject = Scalar . LT.pack
instance FromObject Char LT.Text LT.Text where
    fromObject = helper . LT.unpack <=< fromScalar where
        helper [x] = return x
        helper x = failure $ ExpectedSingleCharacter x
    listFromObject = fmap LT.unpack . fromScalar

-- Day
instance ConvertSuccess Day LT.Text where
    convertSuccess = LT.pack . show
instance ConvertAttempt Day LT.Text where
    convertAttempt = return . convertSuccess
instance ToObject Day k LT.Text where
    toObject = scalarToObject
instance ConvertAttempt LT.Text Day where
    convertAttempt t = do -- FIXME
        let s = LT.unpack t
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
instance FromObject Day k LT.Text where
    fromObject = scalarFromObject

-- Bool
instance ConvertAttempt Bool LT.Text where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Bool LT.Text where
    convertSuccess b = LT.pack $ if b then "true" else "false"
instance ToObject Bool k LT.Text where
    toObject = scalarToObject
instance ConvertAttempt LT.Text Bool where
    convertAttempt t =
        case LT.unpack t of
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
instance FromObject Bool k LT.Text where
    fromObject = scalarFromObject

-- Int
instance ConvertSuccess Int LT.Text where
    convertSuccess = LT.pack . show
instance ConvertAttempt Int LT.Text where
    convertAttempt = return . convertSuccess
instance ToObject Int k LT.Text where
    toObject = scalarToObject
instance ConvertAttempt LT.Text Int where
    convertAttempt = SF.read . LT.unpack
instance FromObject Int k LT.Text where
    fromObject = scalarFromObject

-- Rational
instance ConvertSuccess (Ratio Integer) LT.Text where
    convertSuccess = LT.pack . show
instance ConvertAttempt (Ratio Integer) LT.Text where
    convertAttempt = return . convertSuccess
instance ToObject (Ratio Integer) k LT.Text where
    toObject = scalarToObject
instance ConvertAttempt LT.Text (Ratio Integer) where
    convertAttempt = SF.read . LT.unpack
instance FromObject (Ratio Integer) k LT.Text where
    fromObject = scalarFromObject

-- | 'toObject' specialized for 'TextObject's
toTextObject :: ToObject a LT.Text LT.Text => a -> TextObject
toTextObject = toObject

-- | 'fromObject' specialized for 'TextObject's
fromTextObject :: FromObject a LT.Text LT.Text => TextObject -> Attempt a
fromTextObject = fromObject

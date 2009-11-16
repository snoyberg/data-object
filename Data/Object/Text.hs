{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Time.Calendar
import Control.Monad ((<=<))
import Data.Ratio (Ratio)
import Data.Attempt
import Data.Generics
import Control.Exception (Exception)
import Data.Convertible
import qualified Safe.Failure as SF

-- | 'Object's with keys and values of type 'LT.Text'.
type TextObject = Object LT.Text LT.Text

instance ToObject BL.ByteString a LT.Text where
    toObject = scalarToObject
instance FromObject BL.ByteString a LT.Text where
    fromObject = scalarFromObject

instance ToObject BS.ByteString a LT.Text where
    toObject = scalarToObject
instance FromObject BS.ByteString a LT.Text where
    fromObject = scalarFromObject

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

data InvalidDayException = InvalidDayException String
    deriving (Show, Typeable)
instance Exception InvalidDayException
instance ConvertAttempt LT.Text Day where
    convertAttempt t = do
        let s = LT.unpack t
        SF.assert (length s == 10) () $ InvalidDayException s
        wrapFailure (const $ InvalidDayException s) $ do
            y <- SF.read $ take 4 s
            m <- SF.read $ take 2 $ drop 5 s
            d <- SF.read $ take 2 $ drop 8 s
            return $ fromGregorian y m d
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

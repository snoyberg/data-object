module Data.Scalar where

import Data.ByteString.Lazy (ByteString, empty)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Object.Raw (ToRaw (..), ToRawObject (..))
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

data Scalar = Numeric   Rational
            | Text      Text
            | Binary    ByteString
            | Bool      Bool
            | Timestamp UTCTime
            | Null

instance ToRawObject Scalar where
    toRawObject = toRawObject . toRaw

instance ToRaw Scalar where
    toRaw (Numeric n) = toRaw $ show n
    toRaw (Text t) = toRaw $ encodeUtf8 t
    toRaw (Binary b) = toRaw b
    toRaw (Bool True) = toRaw "true"
    toRaw (Bool False) = toRaw "false"
    -- this is W3 format for timestamps.
    toRaw (Timestamp t) =
        toRaw $ formatTime defaultTimeLocale "%FT%XZ" t
    toRaw Null = toRaw empty

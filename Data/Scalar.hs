module Data.Scalar where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Time.Clock (UTCTime)

data Scalar = Numeric   Rational
            | Text      Text
            | Binary    ByteString
            | Bool      Bool
            | Timestamp UTCTime
            | Null


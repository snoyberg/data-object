{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module enables all the dangerous instances that make this library
-- easy to use. Caveat emptor!
module Data.Object.Dangerous
    (
    ) where

import Data.Object.Base
import Data.Convertible.Text

instance ToObject x k v => ConvertSuccess x (Object k v) where
    convertSuccess = toObject
{- This is too dangerous even for Dangerous ;).
 - It requires IncoherentInstances, since we have
 - ConvertSuccess => ConvertAttempt in convertible-text
instance FromObject x k v => ConvertAttempt (Object k v) x where
    convertAttempt = fromObject
-}

instance ToObject (Object kIn vIn) kOut vOut
    => FromObject (Object kOut vOut) kIn vIn where
    fromObject = return . toObject

instance (ConvertSuccess kIn kOut, ConvertSuccess vIn vOut)
    => ToObject (Object kIn vIn) kOut vOut where
    toObject = mapKeysValues convertSuccess convertSuccess

instance ToObject v k v where
    toObject = Scalar
instance FromObject v k v where
    fromObject = fromScalar

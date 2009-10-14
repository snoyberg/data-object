{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Translate
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Objects which can be translated into different languages.
---------------------------------------------------------
module Data.Object.Translate
    ( -- * Types
      Language
    , TranslatedString
    , Translator
    , TranslateObject
    , TranslateKeyObject
      -- * Type classes
    , CanTranslate
      -- * Utilities for objects
    , translateObject
    , translateKeyObject
    ) where

import Data.Maybe (fromMaybe)
import Data.Object

-- | Should usually be the well established I18N translation code. Examples
-- include en, en_US, es, and so on. If you use these common codes, you will
-- have easy interop with other systems.
type Language = String
type TranslatedString = String

-- | Given a list of destination languages (ordered by preference), generate
-- a translated string. Must return some value.
type Translator = [Language] -> TranslatedString

-- | Usually you do not need to translate both keys and values, so this should
-- be the more common type.
type TranslateObject = Object String Translator

-- | For the occassions when you really need to translate everything.
type TranslateKeyObject = Object Translator Translator

-- | Anything which can be translated into a different language.
--
-- Minimal complete definition: translate or tryTranslate and
-- defaultTranslate.
class CanTranslate a where
    translate :: a -> Translator
    translate a [] = defaultTranslate a
    translate a (lang:langs) =
        fromMaybe (translate a langs) $ tryTranslate a lang

    tryTranslate :: a -> Language -> Maybe TranslatedString
    tryTranslate a = Just . translate a . return

    defaultTranslate :: a -> TranslatedString
    defaultTranslate a = translate a []

instance CanTranslate [Char] where
    translate = const

translateObject :: [Language]
                -> TranslateObject
                -> Object String String
translateObject langs = fmap ($ langs)

translateKeyObject :: [Language]
                   -> TranslateKeyObject
                   -> Object String String
translateKeyObject langs = mapKeysValues ($ langs) ($ langs)

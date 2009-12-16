{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------
--
-- Module        : Data.Object.Base
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- These objects show up in different places, eg JSON, Yaml.
-- By providing a representation in a separate repository,
-- other libraries can share a single representation of
-- these structures.
--
---------------------------------------------------------

-- | The core of this package is the 'Object' data type, which is used for
-- handling scalars, sequences and mappings in a nested manner. This
-- is the same structure used in JSON or Yaml data.
--
-- The 'Object' data type is polymorphic in its keys and values. Submodules
-- within this package provide more concrete datatypes, such as a 'String'
-- 'Object' and a specialized scalar type.
--
-- Besides the 'Object' data type, there are utility functions and type classes
-- for converting objects around. Care has been taken to avoid any overloaded
-- instances for these type classes.
module Data.Object.Base
    ( -- * Object data type
      Object (..)
      -- * Basic mapping of keys and values
    , mapKeys
    , mapValues
    , mapKeysValues
    , mapKeysValuesA
    , mapKeysValuesM
      -- * Convert entires objects
    , convertObject
    , convertObjectM
      -- * Extracting underlying values
    , ObjectExtractError (..)
    , fromScalar
    , fromSequence
    , fromMapping
      -- * Common object conversions
    , sTO
    , sFO
    , lTO
    , lFO
    , mTO
    , mFO
    , olTO
    , olFO
    , omTO
    , omFO
      -- * Automatic deriving of instances
    , deriveSuccessConvs
      -- * Helper functions
    , lookupObject
      -- * Re-export
    , module Data.Convertible.Text
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad (ap, (<=<))

import Prelude hiding (mapM, sequence)

import Data.Foldable hiding (concatMap, concat)
import Data.Traversable
import Data.Monoid

import Data.Generics
import qualified Safe.Failure as A
import Control.Exception (Exception)
import Data.Attempt

import Data.Convertible.Text
import Language.Haskell.TH

-- | Can represent nested values as scalars, sequences and mappings.  A
-- sequence is synonymous with a list, while a mapping is synonymous with a
-- list of pairs.
--
-- Note that instances of standard library type classes for this data type
-- leave the key untouched while altering the value. For example, the 'Functor'
-- instance defines 'fmap' to be synonymous with 'mapValues'.
data Object key val =
    Mapping [(key, Object key val)]
    | Sequence [Object key val]
    | Scalar val
    deriving (Show, Eq, Data, Typeable)

instance Functor (Object key) where
    fmap = mapValues

instance Foldable (Object key) where
    foldMap f (Scalar v) = f v
    foldMap f (Sequence vs) = mconcat $ map (foldMap f) vs
    foldMap f (Mapping pairs) = mconcat $ map (foldMap f . snd) pairs

instance Traversable (Object key) where
    traverse f (Scalar v) = Scalar <$> f v
    traverse f (Sequence vs) = Sequence <$> traverse (traverse f) vs
    traverse f (Mapping pairs) =
      Mapping <$> traverse (traverse' (traverse f)) pairs

-- It would be nice if there were an "instance Traversable ((,) a)", but I
-- won't make an orphan instance simply for convenience. Instead:
traverse' :: Applicative f => (a -> f b) -> (x, a) -> f (x, b)
traverse' f (x, a) = (,) x <$> f a

joinObj :: Object key (Object key scalar) -> Object key scalar
joinObj (Scalar x)    = x
joinObj (Sequence xs) = Sequence (map joinObj xs)
joinObj (Mapping  xs) = Mapping  (map (second joinObj) xs)

instance Monad (Object key) where
    return = Scalar
    x >>= f = joinObj . fmap f $ x

instance Applicative (Object key) where
    pure  = Scalar
    (<*>) = ap

-- | Apply some conversion to the keys of an 'Object', leaving the values
-- unchanged.
mapKeys :: (keyIn -> keyOut) -> Object keyIn val -> Object keyOut val
mapKeys = flip mapKeysValues id

-- | Apply some conversion to the values of an 'Object', leaving the keys
-- unchanged. This is equivalent to 'fmap'.
mapValues :: (valIn -> valOut) -> Object key valIn -> Object key valOut
mapValues = mapKeysValues id

-- | Apply a conversion to both the keys and values of an 'Object'.
mapKeysValues :: (keyIn -> keyOut)
              -> (valIn -> valOut)
              -> Object keyIn valIn
              -> Object keyOut valOut
mapKeysValues _ fv (Scalar v) = Scalar $ fv v
mapKeysValues fk fv (Sequence os)= Sequence $ map (mapKeysValues fk fv) os
mapKeysValues fk fv (Mapping pairs) =
    Mapping $ map (fk *** mapKeysValues fk fv) pairs

-- | Apply an 'Applicative' conversion to both the keys and values of an
-- 'Object'.
mapKeysValuesA :: Applicative f
               => (keyIn -> f keyOut)
               -> (valIn -> f valOut)
               -> Object keyIn valIn
               -> f (Object keyOut valOut)
mapKeysValuesA _ fv (Scalar v) = Scalar <$> fv v
mapKeysValuesA fk fv (Sequence os) =
    Sequence <$> traverse (mapKeysValuesA fk fv) os
mapKeysValuesA fk fv (Mapping pairs) = Mapping <$>
    traverse (uncurry (liftA2 (,)) . (fk *** mapKeysValuesA fk fv)) pairs

-- | The same as 'mapKeysValuesA', but using a 'Monad' since some people are
-- more comfortable with 'Monad's and not all 'Monad's are 'Applicative'.
mapKeysValuesM :: Monad m
               => (keyIn -> m keyOut)
               -> (valIn -> m valOut)
               -> Object keyIn valIn
               -> m (Object keyOut valOut)
mapKeysValuesM fk fv =
    let fk' = WrapMonad . fk
        fv' = WrapMonad . fv
     in unwrapMonad . mapKeysValuesA fk' fv'

convertObject :: (ConvertSuccess k k', ConvertSuccess v v')
              => Object k v
              -> Object k' v'
convertObject = mapKeysValues cs cs

convertObjectM :: (ConvertAttempt k k', ConvertAttempt v v')
               => Object k v
               -> Attempt (Object k' v')
convertObjectM = mapKeysValuesM ca ca

-- | An error value returned when an unexpected node is encountered, eg you
-- were expecting a 'Scalar' and found a 'Mapping'.
data ObjectExtractError =
    ExpectedScalar
    | ExpectedSequence
    | ExpectedMapping
    deriving (Typeable, Show)
instance Exception ObjectExtractError

-- | Extra a scalar from the input, failing if the input is a sequence or
-- mapping.
fromScalar :: MonadFailure ObjectExtractError m => Object k v -> m v
fromScalar (Scalar s) = return s
fromScalar _ = failure ExpectedScalar

-- | Extra a sequence from the input, failing if the input is a scalar or
-- mapping.
fromSequence :: MonadFailure ObjectExtractError m
             => Object k v
             -> m [Object k v]
fromSequence (Sequence s) = return s
fromSequence _ = failure ExpectedSequence

-- | Extra a mapping from the input, failing if the input is a scalar or
-- sequence.
fromMapping :: MonadFailure ObjectExtractError m
            => Object k v
            -> m [(k, Object k v)]
fromMapping (Mapping m) = return m
fromMapping _ = failure ExpectedMapping

sTO :: ConvertSuccess v v' => v -> Object k v'
sTO = Scalar . cs

sFO :: ConvertAttempt v' v => Object k v' -> Attempt v
sFO = ca <=< fromScalar

lTO :: ConvertSuccess v v' => [v] -> Object k v'
lTO = Sequence . map (Scalar . cs)

lFO :: ConvertAttempt v' v => Object k v' -> Attempt [v]
lFO = mapM (ca <=< fromScalar) <=< fromSequence

mTO :: (ConvertSuccess k k', ConvertSuccess v v')
                => [(k, v)]
                -> Object k' v'
mTO = Mapping . map (cs *** Scalar . cs)

mFO :: (ConvertAttempt k' k, ConvertAttempt v' v)
                  => Object k' v'
                  -> Attempt [(k, v)]
mFO =
    mapM (runKleisli (Kleisli ca *** Kleisli sFO))
 <=< fromMapping

olTO :: ConvertSuccess x (Object k v) => [x] -> Object k v
olTO = Sequence . map cs

olFO :: ConvertAttempt (Object k v) x => Object k v -> Attempt [x]
olFO = mapM ca <=< fromSequence

omTO :: (ConvertSuccess k' k, ConvertSuccess x (Object k v))
                 => [(k', x)]
                 -> Object k v
omTO = Mapping . map (cs *** cs)

omFO :: (ConvertAttempt k k', ConvertAttempt (Object k v) x)
                   => Object k v
                   -> Attempt [(k', x)]
omFO = mapM (runKleisli (Kleisli ca *** Kleisli ca)) <=< fromMapping

deriveSuccessConvs :: Name -- ^ dest key
                   -> Name -- ^ dest value
                   -> [Name] -- ^ source keys
                   -> [Name] -- ^ source values
                   -> Q [Dec]
deriveSuccessConvs dk dv sks svs = do
    sto <- [|sTO|]
    sfo <- [|sFO|]
    lto <- [|lTO|]
    lfo <- [|lFO|]
    mto <- [|mTO|]
    mfo <- [|mFO|]
    olto <- [|olTO|]
    olfo <- [|olFO|]
    omto <- [|omTO|]
    omfo <- [|omFO|]
    co <- [|convertObject|]
    coa <- [|convertObjectM|]
    let sks' = map ConT sks
        svs' = map ConT svs
        pairs = do
            sk <- sks'
            sv <- svs'
            return (sk, sv)
    let valOnly = concatMap (helper1 sto sfo lto lfo) svs'
        both = concatMap (helper2 mto mfo olto olfo co coa omto omfo) pairs
        keyOnly = concatMap (helper3 omto omfo) sks'
    return $ valOnly ++ both ++ keyOnly
      where
        dk' = ConT dk
        dv' = ConT dv
        objectt k v = ConT (mkName "Object") `AppT` k `AppT` v
        to' src = ConT (mkName "ConvertSuccess") `AppT` src `AppT`
                  objectt dk' dv'
        fo' dst = ConT (mkName "ConvertAttempt") `AppT`
                  objectt dk' dv' `AppT` dst
        cs' = mkName "convertSuccess"
        ca' = mkName "convertAttempt"
        to src f =
            InstanceD [] (to' src) [FunD cs' [Clause [] (NormalB f) []]]
        fo dst f =
            InstanceD [] (fo' dst) [FunD ca' [Clause [] (NormalB f) []]]
        tofo ty x y = [to ty x, fo ty y]
        listt = AppT ListT
        pairt k v = TupleT 2 `AppT` k `AppT` v
        helper1 sto sfo lto lfo sv = concat
            [ tofo sv sto sfo
            , tofo (listt sv) lto lfo
            ]
        helper2 mto mfo olto olfo co coa omto omfo (sk, sv) = concat
            [ tofo (listt $ pairt sk sv) mto mfo
            , tofo (listt $ objectt sk sv) olto olfo
            , if sk == dk' && sv == dv' -- avoid overlapping with identity
                then []
                else tofo (objectt sk sv) co coa
            , if sk == dk' && sv == dv' -- avoid overlapping with helper3
                then []
                else tofo (listt $ pairt sk $ objectt sk sv) omto omfo
            ]
        helper3 omto omfo sk = concat
            [ tofo (listt $ pairt sk $ objectt dk' dv') omto omfo
            ]

-- | An equivalent of 'lookup' to deal specifically with maps of 'Object's. In
-- particular, it will:
--
-- 1. Automatically convert the lookup key as necesary. For example- assuming
-- you have the appropriate 'ConvertSuccess' instances, you could lookup an 'Int' in
-- a map that has 'String' keys.
--
-- 2. Return the result in an 'Attempt', not 'Maybe'. This is especially useful
-- when creating 'FromObject' instances.
--
-- 3. Show a more useful error message. Since this function requires the key to
-- be 'Show'able, the fail message states what key was not found.
--
-- 4. Calls 'fromObject' automatically, so you get out the value type that you
-- want, not just an 'Object'.
lookupObject :: ( ConvertSuccess k' k
                , ConvertAttempt (Object k v) o
                , Typeable k
                , Typeable v
                , Show k
                , Eq k
                )
             => k'
             -> [(k, Object k v)]
             -> Attempt o
lookupObject key = ca <=< A.lookup (convertSuccess key)

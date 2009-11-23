{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
---------------------------------------------------------
--
-- Module        : Data.Object
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
module Data.Object
    ( -- * Object data type
      Object (..)
      -- * Basic mapping of keys and values
    , mapKeys
    , mapValues
    , mapKeysValues
    , mapKeysValuesA
    , mapKeysValuesM
      -- * Extracting underlying values
    , ObjectExtractError (..)
    , fromScalar
    , fromSequence
    , fromMapping
      -- * Higher level conversions
    , ToObject (..)
    , FromObject (..)
      -- ** Wrapping 'FromObject'
    , FromObjectException (..)
    , fromObjectWrap
      -- * Helper functions
    , lookupObject
      -- ** Scalar/Object conversions
      -- $scalarToFromObject
    , scalarToObject
    , scalarFromObject
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad (ap, (<=<))

import Prelude hiding (mapM, sequence)

import Data.Foldable
import Data.Traversable
import Data.Monoid

import Data.Generics
import qualified Safe.Failure as A
import Control.Exception (Exception)
import Data.Attempt

import Data.Convertible

-- | Can represent nested values as scalars, sequences and mappings.  A
-- sequence is synonymous with a list, while a mapping is synonymous with a
-- list of pairs.
--
-- Note that instances of this data type for standard library type classes for
-- the most part leave the key untouched while altering the value. For example,
-- the 'Functor' instance defines 'fmap' to be synonymous with 'mapValues'.
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
fromScalar :: Object k v -> Attempt v
fromScalar (Scalar s) = return s
fromScalar _ = failure ExpectedScalar

-- | Extra a sequence from the input, failing if the input is a scalar or
-- mapping.
fromSequence :: Object k v -> Attempt [Object k v]
fromSequence (Sequence s) = return s
fromSequence _ = failure ExpectedSequence

-- | Extra a mapping from the input, failing if the input is a scalar or
-- sequence.
fromMapping :: Object k v -> Attempt [(k, Object k v)]
fromMapping (Mapping m) = return m
fromMapping _ = failure ExpectedMapping

-- | Something which can be converted from a to 'Object' k v with guaranteed
-- success. A somewhat unusual but very simple example would be:
--
-- @
--    data TestScore = TestScore { name :: String, score :: Int }
--    instance ToObject [TestScore] String Int where
--        {- Explicit version, slightly tedious
--        toObject = Mapping . map (name &&& Scalar . score)
--        -}
--        {- Or, just let toObject figure it out for you! -}
--        toObject = toObject . map (name &&& score)
-- @
--
-- Then toObject [TestScore \"Abe\" 5, TestScore \"Bill\" 2] would produce, in
-- JSON format, {\"Abe\":5,\"Bill\":2}.
--
-- The purpose of showing these two versions of the implementation are to give
-- an idea of the power of 'toObject'. Since many basic instances of 'ToObject'
-- are included, you can often times avoid using the 'Object' constructors
-- directly and simply call 'toObject'.
--
-- In the first version of the code, we explicitly convert each TestScore into a
-- (String, Object String Int); notice how we must use \"Scalar . score\". We
-- then need to wrap that whole structure into a 'Mapping' constructor.
--
-- In the second version, we just convert each TestScore into a ('String',
-- 'Int') pair, then use a built-in instance of 'ToObject' to convert [(k, v)]
-- into Object k v.
--
-- Please read the documentation on 'FromObject' to see how this same approach
-- is used on the reverse end of the conversion for an even more powerful
-- result.
--
-- Minimal complete definition: 'toObject'.
class ToObject a k v where
    toObject :: a -> Object k v

    listToObject :: [a] -> Object k v
    listToObject = Sequence . map toObject

    -- | This isn't useful for any of the instances we define here, but
    -- other users may find uses for it.
    mapToObject :: ConvertSuccess k' k => [(k', a)] -> Object k v
    mapToObject = Mapping . map (convertSuccess *** toObject)

-- | Something which can attempt a conversion from 'Object' k v to a with a
-- possibility of failure. To finish off with the example in 'ToObject':
--
-- @
--      data TestScore = TestScore { name :: String, score :: Int }
--      instance FromObject [TestScore] String Int where
--          {- Verbose, simple version
--          fromObject o = do
--              objectPairs <- getMapping o
--              pairs <- mapM getScalarFromSecond objectPairs
--              return $ map testScoreFromPair pairs
--              where
--                  getScalarFromSecond :: (k, Object k v)
--                                      -> Attempt (k, v)
--                  getScalarFromSecond (k, v) = do
--                      v' <- getScalar v
--                      return (k, v')
--                  testScoreFromPair :: (String, Int) -> TestScore
--                  testScoreFromPair (n, s) = TestScore n s
--          -}
--          {- Complicated, short version
--          fromObject =
--              mapM (fmap (uncurry TestScore)
--                   . runKleisli (second $ Kleisli getScalar))
--              <=< getMapping
--          -}
--          {- And this is just cheating -}
--          fromObject o = map (uncurry TestScore) `fmap` fromObject o
-- @
--
-- Hopefully this example demonstrates how powerful an idea fromObject can be.
-- In this example, there are two things that could cause problems with the
-- data:
--
-- 1. The initial value may not be a 'Mapping'.
--
-- 2. Given that it is a 'Mapping', one of its values may not be a 'Scalar'.
--
-- Starting with the verbose version, we use 'getMapping' to ensure that we are
-- dealing with a 'Mapping' and 'getScalarFromSecond' to ensure that all values
-- in that 'Mapping' are in fact 'Scalar's. In the complicated version, we do
-- the exact same thing, instead using 'Kleisli' arrows to do the heavy lifting
-- in tuples.
--
-- However, the \"cheating\" version simply (ab)uses the fact that there are
-- already instances of 'FromObject' to deal with conversion from 'Object' k v
-- to [(k, v)]. The only thing left is to convert those pairs into
-- 'TestScore's.
--
-- Minimal complete definition: 'fromObject'.
class FromObject a k v where
    fromObject :: Object k v -> Attempt a

    listFromObject :: Object k v -> Attempt [a]
    listFromObject = mapM fromObject <=< fromSequence

    -- | This isn't useful for any of the instances we define here, but
    -- other users may find uses for it.
    mapFromObject :: ConvertAttempt k k'
                  => Object k v
                  -> Attempt [(k', a)]
    mapFromObject =
        mapM (runKleisli (Kleisli convertAttempt *** Kleisli fromObject))
         <=< fromMapping

-- Object identity conversions
instance ToObject (Object k v) k v where
    toObject = id
instance FromObject (Object k v) k v where
    fromObject = return

-- The following code seems too generic and will probably lead to overlapping
-- instances. It has thus been commented out.
{-
-- Converting between different types of Objects
instance (ConvertSuccess k k', ConvertSuccess v v')
  => ToObject (Object k v) k' v' where
    toObject = mapKeysValues convertSuccess convertSuccess

instance (ConvertAttempt k' k, ConvertAttempt v' v)
  => FromObject (Object k v) k' v' where
    fromObject = mapKeysValuesM convertAttempt convertAttempt
-}

-- Sequence
instance ToObject a k v => ToObject [a] k v where
    toObject = listToObject
instance FromObject a k v => FromObject [a] k v where
    fromObject = listFromObject

-- Mapping
instance (ConvertSuccess k k', ToObject v k' v') => ToObject (k, v) k' v' where
    toObject = listToObject . return
    listToObject = Mapping . map (convertSuccess *** toObject)
instance (ConvertAttempt k' k, FromObject v k' v') => FromObject (k, v) k' v' where
    fromObject o = do
        ms <- listFromObject o
        case ms of
            [m] -> return m
            _ -> failureString "fromObject of pair requires mapping of size 1"
    listFromObject =
        mapM (runKleisli (Kleisli convertAttempt *** Kleisli fromObject))
        <=< fromMapping

-- | Wraps any 'Exception' thrown during a 'fromObject' call.
data FromObjectException = forall e. Exception e => FromObjectException e
    deriving Typeable
instance Show FromObjectException where
    show (FromObjectException e) = "FromObjectException " ++ show e
instance Exception FromObjectException

-- | Calls 'fromObject' and wraps any 'Exception's in a 'FromObjectException'.
fromObjectWrap :: (FromObject x k y, MonadFailure FromObjectException m)
               => Object k y
               -> m x
fromObjectWrap = attempt (failure . FromObjectException) return . fromObject

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
                , FromObject o k v
                , Typeable k
                , Typeable v
                , Show k
                , Eq k
                )
             => k'
             -> [(k, Object k v)]
             -> Attempt o
lookupObject key pairs = A.lookup (convertSuccess key) pairs >>= fromObject

-- $scalarToFromObject
-- Due to overlapping instances, we cannot automatically make all instances of
-- 'ConvertSuccess' instances of 'ToObject' (and same with
-- 'ConvertAttempt'/'FromObject'), even though the implementation is standard. Just
-- use the following functions whenever you declare 'ConvertSuccess'/'ConvertAttempt'
-- instance and you should be good.

-- | An appropriate 'toObject' function for any types x and y which have a
-- 'ConvertSuccess' x y instance.
scalarToObject :: ConvertSuccess x y => x -> Object k y
scalarToObject = Scalar . convertSuccess

-- | An appropriate 'fromObject' function for any types x and y which have a
-- 'ConvertAttempt' x y instance.
scalarFromObject :: ConvertAttempt y x => Object k y -> Attempt x
scalarFromObject = convertAttempt <=< fromScalar

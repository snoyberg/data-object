{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    , getScalar
    , getSequence
    , getMapping
      -- * Higher level conversions
      -- $toFromDesc
    , ToScalar (..)
    , FromScalar (..)
    , ToObject (..)
    , FromObject (..)
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
import qualified Control.Exception as E
import Data.Attempt

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
instance E.Exception ObjectExtractError

-- | Extra a scalar from the input, failing if the input is a sequence or
-- mapping.
getScalar :: Object k v -> Attempt v
getScalar (Scalar s) = return s
getScalar _ = failure ExpectedScalar

-- | Extra a sequence from the input, failing if the input is a scalar or
-- mapping.
getSequence :: Object k v -> Attempt [Object k v]
getSequence (Sequence s) = return s
getSequence _ = failure ExpectedSequence

-- | Extra a mapping from the input, failing if the input is a scalar or
-- sequence.
getMapping :: Object k v -> Attempt [(k, Object k v)]
getMapping (Mapping m) = return m
getMapping _ = failure ExpectedMapping

-- $toFromDesc The terminology of to and from below can be a little misleading.
-- It's most logical if we begin by looking at 'ToObject' and 'FromObject'.
-- 'ToObject' is the type for anything which can be converted to an 'Object'
-- with a specified key and value combination. This conversion must be
-- guaranteed to succeed, since the implementor has full knowledge of the type
-- of the object they are converting.
--
-- 'FromObject', on the other hand, might fail. For example, let's say you have the following:
--
-- @
--     data Person = Person { name :: String, age :: Int }
--     instance ToObject Person String String where
--         toObject (Person n a) = Mapping
--             [ (\"name\", Scalar n)
--             , (\"age\", Scalar $ show a)
--             ]
-- @
--
-- Obviously, if 'FromObject' received a value looking like (in JSON notation)
-- {name:\"John\",age:30}, the result should be Person \"John\"
-- 30. However, what do you do with the value {foo:\"bar\"}? That's why the
-- result of 'fromObject' is wrapped in a 'Attempt'.
--
-- 'ToScalar' and 'FromScalar' then borrow the same terminology. Since
-- 'ToScalar' is intended to be called by 'ToObject' instances, it needs to
-- guarantee success; thus no 'Attempt' wrapper. 'FromScalar' simply
-- allows a failure to occur. Using the example from above, if the value passed
-- to fromObject was {name:\"John\",age:\"thirty\"}, the 'fromScalar' call on
-- \"thirty\" should fail, saying that \"thirty\" cannot be converted to an
-- 'Int'.

-- | Something which can be converted from x to y with guaranteed success. For
-- example, every 'Int' can be represented by a 'String', so you could define
-- (with appropriate language extensions):
--
-- @
--      instance ToScalar Int String where
--          toScalar = show
-- @

class ToScalar x y where
    toScalar :: x -> y
-- | Something which can have an attempted conversion from y to x with the
-- possibility of failure. For example, certain 'String's can be converted to
-- 'Int's, so you could define (with appropriate language extensions):
--
-- @
--      instance FromScalar Int String where
--          fromScalar = Data.Attempt.Helper.read
-- @
--
-- Or, you could provide your own error wrapper.
--
-- @
--      newtype InvalidInt = InvalidInt String deriving (Show, Typeable)
--      instance Exception InvalidInt
--      instance FromScalar Int String where
--          fromScalar s =
--              'attempt' ('const' $ 'failure' $ InvalidInt s)
--                      return
--                      ('Data.Attempt.Helper.read' s)
-- @
class FromScalar x y where
    fromScalar :: y -> Attempt x

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

    -- FIXME is this actually necesary?
    mapToObject :: ToScalar k' k => [(k', a)] -> Object k v
    mapToObject = Mapping . map (toScalar *** toObject)

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
    listFromObject = mapM fromObject <=< getSequence

    -- FIXME is this actually necesary?
    mapFromObject :: FromScalar k' k
                  => Object k v
                  -> Attempt [(k', a)]
    mapFromObject =
        mapM (runKleisli (Kleisli fromScalar *** Kleisli fromObject))
         <=< getMapping

-- Identities for To/FromScalar
instance ToScalar k k where toScalar = id
instance FromScalar k k where fromScalar = return

-- Converting between different types of Objects
instance (ToScalar k k', ToScalar v v') => ToObject (Object k v) k' v' where
    toObject = mapKeysValues toScalar toScalar

instance (FromScalar k k', FromScalar v v')
  => FromObject (Object k v) k' v' where
    fromObject = mapKeysValuesM fromScalar fromScalar

-- Sequence
instance ToObject a k v => ToObject [a] k v where
    toObject = listToObject
instance FromObject a k v => FromObject [a] k v where
    fromObject = listFromObject

-- Mapping
instance (ToScalar k k', ToObject v k' v') => ToObject (k, v) k' v' where
    toObject = listToObject . return
    listToObject = Mapping . map (toScalar *** toObject)
instance (FromScalar k k', FromObject v k' v') => FromObject (k, v) k' v' where
    fromObject o = do
        ms <- listFromObject o
        case ms of
            [m] -> return m
            _ -> failureString "fromObject of pair requires mapping of size 1"
    listFromObject =
        mapM (runKleisli (Kleisli fromScalar *** Kleisli fromObject))
        <=< getMapping

-- | An equivalent of 'lookup' to deal specifically with maps of 'Object's. In
-- particular, it will:
--
-- 1. Automatically convert the lookup key as necesary. For example- assuming
-- you have the appropriate 'ToScalar' instances, you could lookup an 'Int' in
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
lookupObject :: ( ToScalar k' k
                , FromObject o k v
                , Typeable k
                , Typeable v
                , Show k
                , Eq k
                )
             => k'
             -> [(k, Object k v)]
             -> Attempt o
lookupObject key pairs = A.lookup (toScalar key) pairs >>= fromObject

-- $scalarToFromObject
-- Due to overlapping instances, we cannot automatically make all instances of
-- 'ToScalar' instances of 'ToObject' (and same with
-- 'FromScalar'/'FromObject'), even though the implementation is standard. Just
-- use the following functions whenever you declare 'ToScalar'/'FromScalar'
-- instance and you should be good.

-- | An appropriate 'toObject' function for any types x and y which have a
-- 'ToScalar' x y instance.
scalarToObject :: ToScalar x y => x -> Object k y
scalarToObject = Scalar . toScalar

-- | An appropriate 'fromObject' function for any types x and y which have a
-- 'FromScalar' x y instance.
scalarFromObject :: FromScalar x y => Object k y -> Attempt x
scalarFromObject = fromScalar <=< getScalar

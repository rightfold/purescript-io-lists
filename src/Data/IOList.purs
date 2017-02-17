module Data.IOList
  ( IOList

  , empty
  , fromByteString
  , fromString
  , fromArray

  , foldl
  , foldM
  , toByteString
  ) where

import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Monoid (class Monoid)
import Node.Encoding (Encoding(..))
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf, resize, stateful)

--------------------------------------------------------------------------------

-- | An I/O list is a sequence of bytes with *O(1)* concatenation.
foreign import data IOList :: Type

instance eqIOList :: Eq IOList where
  eq a b = toByteString a == toByteString b

instance ordIOList :: Ord IOList where
  compare a b = toByteString a `compare` toByteString b

instance showIOList :: Show IOList where
  show a = "(fromByteString " <> show (toByteString a) <> ")"

instance semigroupIOList :: Semigroup IOList where
  append = append'

instance monoidIOList :: Monoid IOList where
  mempty = empty

instance arbitraryIOList :: Arbitrary IOList where
  arbitrary = stateful \{size} -> resize (size / 2) $
    oneOf (pure empty)
        [ fromByteString         <$> arbitrary
        , fromString `flip` UTF8 <$> arbitrary
        , fromArray              <$> arbitrary
        ]

foreign import append' :: IOList -> IOList -> IOList

--------------------------------------------------------------------------------

-- | *O(1)* The empty I/O list.
foreign import empty :: IOList

-- | *O(1)* An I/O list with the bytes from a byte string.
foreign import fromByteString :: ByteString -> IOList

-- | *O(1)* An I/O list with the bytes from an encoded string.
foreign import fromString :: String -> Encoding -> IOList

-- | *O(1)* An I/O list with the bytes from many I/O lists.
foreign import fromArray :: Array IOList -> IOList

--------------------------------------------------------------------------------

-- | *Θ(n)* Fold an I/O list.
foreign import foldl
  :: ∀ a
   . (a -> ByteString -> a)
  -> (a -> String -> Encoding -> a)
  -> a
  -> IOList
  -> a

-- | *Θ(n)* Fold an I/O list.
foldM
  :: ∀ m a
   . (Monad m)
  => (a -> ByteString -> m a)
  -> (a -> String -> Encoding -> m a)
  -> a
  -> IOList
  -> m a
foldM onByteString onString zero = foldl onByteString' onString' (pure zero)
  where
  onByteString' accM bs = accM >>= \acc -> onByteString acc bs
  onString' accM s e = accM >>= \acc -> onString acc s e

-- | *Θ(n)* Fold an I/O list into a byte string.
toByteString :: IOList -> ByteString
toByteString = foldl onByteString onString ByteString.empty
  where
  onByteString = (<>)
  onString b s e = b <> ByteString.fromString s e

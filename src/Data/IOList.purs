module Data.IOList
  ( IOList

  , empty
  , fromByteString
  , fromString
  , fromArray

  , foldl
  ) where

import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Monoid (class Monoid)
import Node.Encoding (Encoding)
import Prelude

--------------------------------------------------------------------------------

-- | An I/O list is a sequence of bytes with *O(1)* concatenation.
foreign import data IOList :: Type

instance semigroupIOList :: Semigroup IOList where
  append = append'

instance monoidIOList :: Monoid IOList where
  mempty = empty

foreign import append' :: IOList -> IOList -> IOList

--------------------------------------------------------------------------------

-- | *O(1)* The empty I/O list.
foreign import empty :: IOList

-- | *O(1)* An I/O list with the bytes from a byte string.
foreign import fromByteString :: ByteString -> IOList

-- | An I/O list with the bytes from an encoded string.
-- |
-- |  - *O(1)* with JavaScript backends.
-- |  - *Θ(n)* with Erlang backends.
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

-- | *Θ(n)* Fold an I/O list into a byte string.
toByteString :: IOList -> ByteString
toByteString = toByteString' $ foldl onByteString onString ByteString.empty
  where
  onByteString = (<>)
  onString b s e = b <> ByteString.fromString s e

-- |  - Equivalent to `($)` with JavaScript backends.
-- |  - Will ignore the first argument and call `iolist_to_binary/1` with
-- |    Erlang backends.
foreign import toByteString' :: (IOList -> ByteString) -> IOList -> ByteString
module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.IOList (IOList, empty, fromArray, fromByteString, fromString, isEmpty, length, toByteString, write)
import Node.Encoding (Encoding(..))
import Node.Stream (end, Writable)
import Prelude
import Test.QuickCheck ((===))
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

main = runTest do
  test "Monoid"    $ liftEff $ checkMonoid    (Proxy :: Proxy IOList)
  test "Semigroup" $ liftEff $checkSemigroup (Proxy :: Proxy IOList)

  test "toByteString <<< fromByteString" $
    quickCheck \b -> toByteString (fromByteString b) === b
  test "fromByteString <<< toByteString" $
    quickCheck \b -> fromByteString (toByteString b) === b

  test "fromString (a <> b) == fromString a <> fromString b" $
    quickCheck \a b ->
      fromString (a <> b) UTF8 === fromString a UTF8 <> fromString b UTF8

  test "fromArray [a, b] == a <> b" $
    quickCheck \a b -> fromArray [a, b] === a <> b

  test "isEmpty empty" $
    quickCheck $ isEmpty empty

  test "isEmpty (fromByteString ByteString.empty)" $
    quickCheck $ isEmpty (fromByteString ByteString.empty)

  test "isEmpty (fromString \"\" UTF8)" $
    quickCheck $ isEmpty (fromString "" UTF8)

  test "isEmpty (fromArray [])" $
    quickCheck $ isEmpty (fromArray [])

  test "isEmpty (fromArray [empty])" $
    quickCheck $ isEmpty (fromArray [empty])

  test "length empty == 0" $
    quickCheck $ length empty === 0

  test "length a == ByteString.length (toByteString a)" $
    quickCheck \a -> length a === ByteString.length (toByteString a)

  test "write" $
    quickCheck \a -> unsafePerformEff do
      stream <- newMemoryStream
      _ <- write stream a (pure unit)
      end stream (pure unit)
      b <- memoryStreamToByteString stream
      pure $ toByteString a == b

foreign import data Memory :: Type

foreign import newMemoryStream
  :: ∀ eff
   . Eff (ref :: REF | eff) (Writable (memory :: Memory) (ref :: REF | eff))

foreign import memoryStreamToByteString
  :: ∀ r eff
   . Writable (memory :: Memory | r) (ref :: REF | eff)
  -> Eff (ref :: REF | eff) ByteString

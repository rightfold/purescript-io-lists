module Test.Main
  ( main
  ) where

import Control.Monad.Eff.Class (liftEff)
import Data.IOList (IOList, fromArray, fromByteString, fromString, toByteString)
import Node.Encoding (Encoding(..))
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

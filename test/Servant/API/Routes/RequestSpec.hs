module Servant.API.Routes.RequestSpec
  ( spec
  )
where

import Servant.API.Routes.Internal.Request
import "this" Servant.API.Routes.SomeSpec hiding (spec)
import Servant.API.Routes.Util
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q

{- hlint ignore "Monoid law, right identity" -}
{- hlint ignore "Monoid law, left identity" -}
{- hlint ignore "Use fold" -}

instance Q.Arbitrary Request where
  arbitrary = Request <$> genSome (Q.elements [intTypeRep, strTypeRep, unitTypeRep])
  shrink = unRequest (shrinkSome (const []))

spec :: Spec
spec = do
  describe "Semigroup/Monoid laws" $ do
    prop "Associativity" $ do
      \(x :: Request, y, z) -> x <> (y <> z) === (x <> y) <> z
    prop "Right identity" $
      \(x :: Request) -> x <> mempty === x
    prop "Left identity" $ do
      \(x :: Request) -> mempty <> x === x
    prop "Concatentation" $ do
      \(xs :: [Request]) -> mconcat xs === foldr (<>) mempty xs
  it "AllTypeable" $ do
    typeReps @'[Int, String] `shouldMatchList` [intTypeRep, strTypeRep]

module Servant.API.Routes.ResponseSpec
  ( spec
  )
where

import qualified Data.Set as Set
import Servant.API.Routes.Internal.Response
import Servant.API.Routes.Util
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q
import "this" Servant.API.Routes.HeaderSpec (sampleReps)
import "this" Servant.API.Routes.SomeSpec hiding (spec)

{- hlint ignore "Monoid law, right identity" -}
{- hlint ignore "Monoid law, left identity" -}
{- hlint ignore "Use fold" -}

instance Q.Arbitrary Responses where
  arbitrary = Responses <$> genSome arbitrary
  shrink = unResponses (shrinkSome shrink)

instance Q.Arbitrary Response where
  arbitrary = do
    _responseType <- Q.elements [intTypeRep, strTypeRep, unitTypeRep]
    _responseHeaders <- Set.fromList <$> Q.sublistOf sampleReps
    pure Response {..}
  shrink =
    responseHeaders $
      fmap Set.fromList . Q.shrinkList (const []) . Set.toList

spec :: Spec
spec = do
  describe "Semigroup/Monoid laws" $ do
    prop "Associativity" $ do
      \(x :: Responses, y, z) -> x <> (y <> z) === (x <> y) <> z
    prop "Right identity" $
      \(x :: Responses) -> x <> mempty === x
    prop "Left identity" $ do
      \(x :: Responses) -> mempty <> x === x
    prop "Concatentation" $ do
      \(xs :: [Responses]) -> mconcat xs === foldr (<>) mempty xs
  describe "Hand-rolled instances" $ do
    testEqInstances @Response
    testEqInstances @Responses

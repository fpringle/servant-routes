module Servant.API.Routes.Util where

import Data.Typeable
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H
import qualified Test.QuickCheck as Q

intTypeRep :: TypeRep
intTypeRep = typeRep $ Proxy @Int

strTypeRep :: TypeRep
strTypeRep = typeRep $ Proxy @String

unitTypeRep :: TypeRep
unitTypeRep = typeRep $ Proxy @()

{- HLINT ignore "Use /=" -}

testEqInstances :: forall a. (Q.Arbitrary a, Show a, Eq a) => H.Spec
testEqInstances =
  H.describe "Eq instance should satisfy laws" $ do
    H.prop "Reflexivity" $ \(p1 :: a) ->
      p1 Q.=== p1
    H.prop "Symmetry" $ \(p1 :: a) p2 ->
      (p1 == p2) Q.=== (p2 == p1)
    H.prop "Negation" $ \(p1 :: a) p2 ->
      (p1 /= p2) Q.=== not (p1 == p2)

testOrdInstances :: forall a. (Q.Arbitrary a, Show a, Ord a) => H.Spec
testOrdInstances =
  H.describe "Ord instance should satisfy laws" $ do
    H.prop "Comparability" $ \(p1 :: a) p2 ->
      (p1 <= p2) Q..||. (p2 <= p1)
    H.prop "Transitivity" $ \(p1 :: a) p2 p3 ->
      (p1 <= p2 && p2 <= p3) Q.==> (p1 <= p3)
    H.prop "Reflexivity" $ \(p1 :: a) ->
      p1 <= p1
    H.prop "Antisymmetry" $ \(p1 :: a) p2 ->
      (p1 <= p2 && p2 <= p1) Q.==> (p1 Q.=== p2)

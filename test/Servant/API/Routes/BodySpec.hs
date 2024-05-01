module Servant.API.Routes.BodySpec
  ( spec
  )
where

import Data.Typeable
import Servant.API.Routes.Body
import Servant.API.Routes.Internal.Body
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q

{- hlint ignore "Monoid law, right identity" -}
{- hlint ignore "Monoid law, left identity" -}
{- hlint ignore "Use fold" -}

intTypeRep :: TypeRep
intTypeRep = typeRep $ Proxy @Int

intTypeRepBody :: Body
intTypeRepBody = oneType @Int

strTypeRepBody :: Body
strTypeRepBody = oneType @String

strTypeRep :: TypeRep
strTypeRep = typeRep $ Proxy @String

unitTypeRepBody :: Body
unitTypeRepBody = oneType @()

instance Q.Arbitrary Body where
  arbitrary =
    Q.frequency
      [ (1, pure noBody)
      , (3, Q.elements [intTypeRepBody, strTypeRepBody])
      ,
        ( 3
        , Q.elements
            [ intTypeRepBody <> strTypeRepBody
            , intTypeRepBody <> intTypeRepBody
            ]
        )
      ,
        ( 2
        , Q.elements
            [ intTypeRepBody <> strTypeRepBody <> unitTypeRepBody
            , intTypeRepBody <> unitTypeRepBody <> intTypeRepBody
            ]
        )
      ]
  shrink = fmap listToBody . Q.shrinkList (const []) . bodyToList

spec :: Spec
spec = do
  describe "Smart constructors" $ do
    it "noBody" $ do
      bodyToList noBody `shouldMatchList` []
      listToBody [] `shouldBe` noBody
    it "oneType" $ do
      bodyToList intTypeRepBody `shouldMatchList` [intTypeRep]
      listToBody [intTypeRep] `shouldBe` intTypeRepBody
    it "manyTypes" $ do
      bodyToList (manyTypes @'[Int, String]) `shouldMatchList` [intTypeRep, strTypeRep]
      listToBody [intTypeRep, strTypeRep] `shouldBe` (manyTypes @'[String, Int])
  describe "Semigroup/Monoid laws" $ do
    prop "Associativity" $ do
      \(x :: Body, y, z) -> x <> (y <> z) === (x <> y) <> z
    prop "Right identity" $
      \(x :: Body) -> x <> mempty === x
    prop "Left identity" $ do
      \(x :: Body) -> mempty <> x === x
    prop "Concatentation" $ do
      \(xs :: [Body]) -> mconcat xs === foldr (<>) mempty xs
  it "AllTypeable" $ do
    typeReps @'[Int, String] `shouldMatchList` [intTypeRep, strTypeRep]

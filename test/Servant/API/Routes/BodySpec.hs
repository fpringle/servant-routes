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

genBody :: Q.Gen Body
genBody =
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

shrinkBody :: Body -> [Body]
shrinkBody = fmap listToBody . Q.shrinkList (const []) . bodyToList

gen3Bodies :: Q.Gen (Body, Body, Body)
gen3Bodies = (,,) <$> genBody <*> genBody <*> genBody

shrink3Bodies :: (Body, Body, Body) -> [(Body, Body, Body)]
shrink3Bodies (a, b, c) =
  [(a', b, c) | a' <- shrinkBody a]
    <> [(a, b', c) | b' <- shrinkBody b]
    <> [(a, b, c') | c' <- shrinkBody c]

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
      bodyToList (intTypeRepBody <> strTypeRepBody) `shouldMatchList` [intTypeRep, strTypeRep]
      listToBody [intTypeRep, strTypeRep] `shouldBe` (intTypeRepBody <> strTypeRepBody)
  describe "Semigroup/Monoid laws" $ do
    prop "Associativity" $ do
      Q.forAllShrink gen3Bodies shrink3Bodies $
        \(x, y, z) -> x <> (y <> z) === (x <> y) <> z
    prop "Right identity" $ do
      Q.forAllShrink genBody shrinkBody $
        \x -> x <> mempty === x
    prop "Left identity" $ do
      Q.forAllShrink genBody shrinkBody $
        \x -> mempty <> x === x
    prop "Concatentation" $ do
      Q.forAllShrink (liftArbitrary genBody) (liftShrink shrinkBody) $
        \xs -> mconcat xs === foldr (<>) mempty xs
  it "AllTypeable" $ do
    typeReps @'[Int, String] `shouldMatchList` [intTypeRep, strTypeRep]

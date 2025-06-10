module Servant.API.Routes.SomeSpec
  ( spec
  , genSome
  , shrinkSome
  )
where

import Servant.API.Routes.Internal.Some as S
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q

{- hlint ignore "Monoid law, right identity" -}
{- hlint ignore "Monoid law, left identity" -}
{- hlint ignore "Use fold" -}

genSome :: Q.Gen a -> Q.Gen (Some a)
genSome gen =
  Q.frequency
    [ (1, pure S.None)
    , (2, S.One <$> gen)
    , (4, S.Many . unAtLeast2 <$> genAtLeast2 gen)
    ]

shrinkSome :: (a -> [a]) -> Some a -> [Some a]
shrinkSome shr = fmap S.fromList . Q.shrinkList shr . S.toList

instance (Q.Arbitrary a) => Q.Arbitrary (Some a) where
  arbitrary = genSome arbitrary
  shrink = shrinkSome shrink

newtype AtLeast2 a = AtLeast2 {unAtLeast2 :: [a]}
  deriving (Show, Eq) via [a]

genAtLeast2 :: Q.Gen a -> Q.Gen (AtLeast2 a)
genAtLeast2 gen = do
  a1 <- gen
  a2 <- gen
  as <- Q.listOf gen
  pure . AtLeast2 $ a1 : a2 : as

instance (Arbitrary a) => Arbitrary (AtLeast2 a) where
  arbitrary = genAtLeast2 arbitrary
  shrink = fmap AtLeast2 . filter ((>= 2) . length) . shrink . unAtLeast2

spec :: Spec
spec = do
  describe "Some a <-> [a]" $ do
    it "None" $ testList @Int []
    prop "oneType" $ \(x :: Int) -> testList [x]
    prop "manyTypes" $ \(AtLeast2 (xs :: [Int])) -> testList xs
  where
    testList :: forall a. (Eq a, Show a) => [a] -> Q.Property
    testList lst =
      let some = S.fromList lst
          lst' = S.toList some
      in  lst === lst'

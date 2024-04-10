module Servant.API.Routes.HeaderSpec
  ( spec
  )
where

import Data.Typeable
import Servant.API (Header)
import Servant.API.Routes.Header
import Servant.API.Routes.Internal.Header
import Test.Hspec as H

spec :: Spec
spec = do
  describe "mkHeaderRep" $ do
    it "should work" $ do
      mkHeaderRep @"sym" @Int `shouldBe` HeaderRep "sym" intTypeRep

  describe "GetheaderReps" $ do
    it "should return an empty list for an empty type-level list" $
      getHeaderReps @'[] `shouldBe` []
    it "should recurse properly" $
      getHeaderReps @'[H1, H2, H3] `shouldBe` HeaderRep "h1" intTypeRep : getHeaderReps @'[H2, H3]

intTypeRep :: TypeRep
intTypeRep = typeRep $ Proxy @Int

type H1 = Header "h1" Int

type H2 = Header "h2" Char

type H3 = Header "h3" [Integer]

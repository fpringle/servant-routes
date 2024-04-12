{-# LANGUAGE CPP #-}

module Servant.API.Routes.RouteSpec
  ( spec
  )
where

import Data.Function
import qualified Data.Text as T
import Lens.Micro
import Servant.API.Routes.Internal.Path
import "this" Servant.API.Routes.ParamSpec hiding (spec)
import Servant.API.Routes.Route
import Test.Hspec as H

spec :: Spec
spec = do
  describe "Route" $ do
    describe "showRoute" $ do
      it "renders default route correctly" $
        showRoute (defRoute "POST") `shouldBe` "POST " <> pathSeparator
      it "renders path correctly" $
        let route =
              defRoute "GET"
                & routePath .~ Path ["api", "v2"]
            expected = "GET /api/v2"
        in  showRoute route `shouldBe` expected
      it "renders query params correctly" $
        let route =
              defRoute "PUT"
                & routePath .~ Path ["api", "v2"]
                & routeParams .~ [sing, arrayElem, flag]
            expected = "PUT /api/v2?" <> T.intercalate "&" [singExpected, arrayElemExpected, flagExpected]
        in  showRoute route `shouldBe` expected

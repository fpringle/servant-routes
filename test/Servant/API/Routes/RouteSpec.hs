{-# LANGUAGE CPP #-}

module Servant.API.Routes.RouteSpec
  ( spec
  )
where

import Data.Function
import qualified Data.Text as T
import Lens.Micro
import Network.HTTP.Types.Method
import Servant.API.Routes.BodySpec ()
import Servant.API.Routes.HeaderSpec hiding (spec)
import Servant.API.Routes.Internal.Path
import Servant.API.Routes.Internal.Route
import Servant.API.Routes.ParamSpec hiding (spec)
import Servant.API.Routes.PathSpec (genAlphaText, shrinkText)
import Servant.API.Routes.Route
import Test.Hspec as H
import Test.QuickCheck as Q

instance Q.Arbitrary Route where
  arbitrary = do
    _routeMethod <- renderStdMethod <$> Q.arbitraryBoundedEnum
    _routePath <- arbitrary
    _routeParams <- Q.sublistOf [sing, arrayElem, flag]
    _routeRequestHeaders <- Q.sublistOf sampleReps
    _routeRequestBody <- arbitrary
    _routeResponseHeaders <- Q.sublistOf sampleReps
    _routeResponseType <- arbitrary
    _routeAuths <- Q.listOf genAuths

    pure Route {..}
    where
      genAuths =
        Q.oneof
          [ ("Basic " <>) <$> genAlphaText
          , genAlphaText
          ]

  shrink r =
    routeMethod shrinkMethod r
      <> routePath Q.shrink r
      <> routeParams shrinkSublist r
      <> routeRequestHeaders shrinkSublist r
      <> routeRequestBody Q.shrink r
      <> routeResponseHeaders shrinkSublist r
      <> routeResponseType Q.shrink r
      <> routeAuths (Q.shrinkList shrinkAuth) r
    where
      shrinkMethod = either (const []) (fmap renderStdMethod . Q.shrinkBoundedEnum) . parseMethod
      shrinkSublist = Q.shrinkList (const [])
      shrinkAuth auth = case T.stripPrefix "Basic " auth of
        Nothing -> shrinkText auth
        Just realm -> ("Basic " <>) <$> shrinkText realm

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

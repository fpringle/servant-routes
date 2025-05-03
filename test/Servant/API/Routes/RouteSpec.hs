module Servant.API.Routes.RouteSpec
  ( spec
  )
where

import Data.Function
import qualified Data.Set as Set
import qualified Data.Text as T
import Lens.Micro
import Network.HTTP.Types.Method
import Servant.API.Routes.HeaderSpec hiding (spec)
import Servant.API.Routes.Internal.Auth
import Servant.API.Routes.Internal.Path
import Servant.API.Routes.Internal.Route
import Servant.API.Routes.ParamSpec hiding (spec)
import Servant.API.Routes.PathSpec (genAlphaText, shrinkText)
import Servant.API.Routes.RequestSpec ()
import Servant.API.Routes.ResponseSpec ()
import Servant.API.Routes.Route
import Test.Hspec as H
import Test.QuickCheck as Q

instance Q.Arbitrary Route where
  arbitrary = do
    _routeMethod <- renderStdMethod <$> Q.arbitraryBoundedEnum
    _routePath <- arbitrary
    _routeParams <- Set.fromList <$> Q.sublistOf [sing, arrayElem, flag]
    _routeRequestHeaders <- Set.fromList <$> Q.sublistOf sampleReps
    _routeRequestBody <- arbitrary
    _routeResponse <- arbitrary
    _routeAuths <- Set.fromList <$> Q.listOf genAuths

    pure Route {..}
    where
      genAuths =
        Q.oneof
          [ Basic <$> genAlphaText
          , Custom <$> genAlphaText
          ]

  shrink r =
    routeMethod shrinkMethod r
      <> routePath Q.shrink r
      <> routeParams shrinkSubset r
      <> routeRequestHeaders shrinkSubset r
      <> routeRequestBody Q.shrink r
      <> routeResponse Q.shrink r
      <> routeAuths (shrinkSet shrinkAuth) r
    where
      shrinkMethod = either (const []) (fmap renderStdMethod . Q.shrinkBoundedEnum) . parseMethod
      shrinkSet shr = fmap Set.fromList . Q.shrinkList shr . Set.toList
      shrinkSubset :: Ord a => Set.Set a -> [Set.Set a]
      shrinkSubset = shrinkSet (const [])
      shrinkAuth = \case
        Basic realm -> Basic <$> shrinkText realm
        Custom tag -> Custom <$> shrinkText tag

spec :: Spec
spec = do
  describe "Route" $ do
    describe "renderRoute" $ do
      it "renders default route correctly" $
        renderRoute (defRoute "POST") `shouldBe` "POST " <> pathSeparator
      it "renders path correctly" $
        let route =
              defRoute "GET"
                & routePath .~ Path ["api", "v2"]
            expected = "GET /api/v2"
        in  renderRoute route `shouldBe` expected
      it "renders query params correctly" $
        let route =
              defRoute "PUT"
                & routePath .~ Path ["api", "v2"]
                & routeParams .~ Set.fromList [sing, arrayElem, flag]
            expected = "PUT /api/v2?" <> T.intercalate "&" [singExpected, arrayElemExpected, flagExpected]
        in  renderRoute route `shouldBe` expected

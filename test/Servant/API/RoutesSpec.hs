{-# LANGUAGE CPP #-}

module Servant.API.RoutesSpec
  ( spec
  )
where

import Data.Function
import Data.List
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import Servant.API
import Servant.API.Routes
import Servant.API.Routes.Body
import Servant.API.Routes.Header
import Servant.API.Routes.Internal.Path
import Servant.API.Routes.Param
import "this" Servant.API.Routes.ParamSpec hiding (spec)
import Servant.API.Routes.Path
import Test.Hspec as H

type SubAPI = ReqBody '[JSON] String :> Post '[JSON] Int

type SubAPI2 = Header "h1" T.Text :> "x" :> ("y" :> Put '[JSON] String :<|> SubAPI)

type SubAPI3 =
  Header "h1" T.Text
    :> "x"
    :> ( "y" :> Put '[JSON] String
          :<|> "z" :> Header' '[Optional] "h2" Int :> Get '[JSON] [Integer]
       )

intTypeRepBody :: Body
intTypeRepBody = oneType @Int

infix 1 `shouldBeSorted`

shouldBeSorted :: (HasCallStack, Show a, Eq a, Ord a) => [a] -> [a] -> Expectation
shouldBeSorted = shouldBe `on` sort

#if MIN_VERSION_servant(0,19,0)
data API mode = API
  { ep1 :: mode :- SubAPI
  , ep2 :: mode :- SubAPI2
  , ep3 :: mode :- SubAPI3
  } deriving Generic
#endif

sameRoutes ::
  forall l r.
  (HasRoutes l, HasRoutes r) =>
  Expectation
sameRoutes = getRoutes @l `shouldBeSorted` getRoutes @r

sameRoutesAsSub ::
  forall l.
  (HasRoutes l) =>
  Expectation
sameRoutesAsSub = sameRoutes @l @SubAPI

unchanged ::
  forall l.
  (HasRoutes (l :> SubAPI)) =>
  Expectation
unchanged = sameRoutesAsSub @(l :> SubAPI)

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
  describe "Routes" $ do
    describe "makeRoutes/unmakeRoutes" $
      it "works" pending
  describe "HasRoutes" $ do
    describe "base cases" $ do
      it "EmptyAPI" $ getRoutes @EmptyAPI `shouldBeSorted` []
      it "UVerb" $ pendingWith "Need to implement issue #5"
      it "Verb" $ do
        getRoutes @(Post '[JSON] Int) `shouldBeSorted` [defRoute "POST" & routeResponseType .~ intTypeRepBody]
        getRoutes @(Post '[JSON] (Headers '[Header "h1" String] Int))
          `shouldBeSorted` [ defRoute "POST"
                              & routeResponseType .~ intTypeRepBody
                              & routeResponseHeaders .~ [mkHeaderRep @"h1" @String]
                           ]
      it "Stream" $ do
        getRoutes @(Stream 'POST 201 NoFraming JSON Int) `shouldBeSorted` [defRoute "POST" & routeResponseType .~ intTypeRepBody]

    describe "boring: combinators that don't change routes" $ do
      it "Description" $ unchanged @(Description "desc")
      it "Summary" $ unchanged @(Summary "summary")
      it "Fragment" $ unchanged @(Fragment Int)
      it "Vault" $ unchanged @Vault
      it "HttpVersion" $ unchanged @HttpVersion
      it "IsSecure" $ unchanged @IsSecure
      it "RemoteHost" $ unchanged @RemoteHost
      it "WithNamedContext" $ sameRoutesAsSub @(WithNamedContext "name" '[] SubAPI)

    describe "recursive: some combinators combine or alter routes" $ do
      it ":<|>" $ getRoutes @(SubAPI :<|> SubAPI2) `shouldBeSorted` getRoutes @SubAPI <> getRoutes @SubAPI2
      it "NoContentVerb" $
        showRoute <$> getRoutes @(NoContentVerb 'POST) `shouldBeSorted` ["POST /"]
      it "Symbol :>" $ do
        let prep = routePath %~ prependPathPart "sym"
        getRoutes @("sym" :> SubAPI) `shouldBeSorted` prep <$> getRoutes @SubAPI
        getRoutes @("sym" :> SubAPI2) `shouldBeSorted` prep <$> getRoutes @SubAPI2
        getRoutes @("sym" :> SubAPI3) `shouldBeSorted` prep <$> getRoutes @SubAPI3
      it "Header' :>" $ do
        let addH = routeRequestHeaders %~ (mkHeaderRep @"h1" @Int :)
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI) `shouldBeSorted` addH <$> getRoutes @SubAPI
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI2) `shouldBeSorted` addH <$> getRoutes @SubAPI2
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI3) `shouldBeSorted` addH <$> getRoutes @SubAPI3
        let addHOptional = routeRequestHeaders %~ (mkHeaderRep @"h1" @(Maybe Int) :)
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI) `shouldBeSorted` addHOptional <$> getRoutes @SubAPI
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI2) `shouldBeSorted` addHOptional <$> getRoutes @SubAPI2
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI3) `shouldBeSorted` addHOptional <$> getRoutes @SubAPI3
      it "BasicAuth :>" $ do
        let addAuth = routeAuths %~ ("Basic realm" :)
        getRoutes @(BasicAuth "realm" String :> SubAPI) `shouldBeSorted` addAuth <$> getRoutes @SubAPI
        getRoutes @(BasicAuth "realm" String :> SubAPI2) `shouldBeSorted` addAuth <$> getRoutes @SubAPI2
        getRoutes @(BasicAuth "realm" String :> SubAPI3) `shouldBeSorted` addAuth <$> getRoutes @SubAPI3
      it "AuthProtect :>" $ do
        let addAuth = routeAuths %~ ("my-special-auth" :)
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI) `shouldBeSorted` addAuth <$> getRoutes @SubAPI
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI2) `shouldBeSorted` addAuth <$> getRoutes @SubAPI2
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI3) `shouldBeSorted` addAuth <$> getRoutes @SubAPI3
      it "QueryFlag :>" $ do
        let addFlag = routeParams %~ (flagParam @"sym" :)
        getRoutes @(QueryFlag "sym" :> SubAPI) `shouldBeSorted` addFlag <$> getRoutes @SubAPI
        getRoutes @(QueryFlag "sym" :> SubAPI2) `shouldBeSorted` addFlag <$> getRoutes @SubAPI2
        getRoutes @(QueryFlag "sym" :> SubAPI3) `shouldBeSorted` addFlag <$> getRoutes @SubAPI3
      it "QueryParam' :>" $ do
        let addP = routeParams %~ (singleParam @"h1" @Int :)
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI) `shouldBeSorted` addP <$> getRoutes @SubAPI
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI2) `shouldBeSorted` addP <$> getRoutes @SubAPI2
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI3) `shouldBeSorted` addP <$> getRoutes @SubAPI3
        let addPOptional = routeParams %~ (singleParam @"h1" @(Maybe Int) :)
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI) `shouldBeSorted` addPOptional <$> getRoutes @SubAPI
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI2) `shouldBeSorted` addPOptional <$> getRoutes @SubAPI2
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI3) `shouldBeSorted` addPOptional <$> getRoutes @SubAPI3
      it "QueryParams :>" $ do
        let addP = routeParams %~ (arrayElemParam @"h1" @Int :)
        getRoutes @(QueryParams "h1" Int :> SubAPI) `shouldBeSorted` addP <$> getRoutes @SubAPI
        getRoutes @(QueryParams "h1" Int :> SubAPI2) `shouldBeSorted` addP <$> getRoutes @SubAPI2
        getRoutes @(QueryParams "h1" Int :> SubAPI3) `shouldBeSorted` addP <$> getRoutes @SubAPI3
      it "ReqBody' :>" $ do
        let addB = routeRequestBody <>~ intTypeRepBody
        getRoutes @(ReqBody '[JSON] Int :> SubAPI3) `shouldBeSorted` addB <$> getRoutes @SubAPI3
      it "StreamBody' :>" $ do
        let addB = routeRequestBody <>~ intTypeRepBody
        getRoutes @(StreamBody NoFraming JSON Int :> SubAPI3) `shouldBeSorted` addB <$> getRoutes @SubAPI3
      it "Capture' :>" $ do
        let addC = routePath %~ prependPathPart "<Int>"
        getRoutes @(Capture "cap" Int :> SubAPI) `shouldBeSorted` addC <$> getRoutes @SubAPI
        getRoutes @(Capture "cap" Int :> SubAPI2) `shouldBeSorted` addC <$> getRoutes @SubAPI2
        getRoutes @(Capture "cap" Int :> SubAPI3) `shouldBeSorted` addC <$> getRoutes @SubAPI3
#if MIN_VERSION_servant(0,19,0)
      it "NamedRoutes" $
        getRoutes @(NamedRoutes API) `shouldBeSorted` getRoutes @SubAPI <> getRoutes @SubAPI2 <> getRoutes @SubAPI3
#endif

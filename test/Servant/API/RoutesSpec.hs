{-# LANGUAGE CPP #-}

module Servant.API.RoutesSpec
  ( spec
  )
where

import Data.Function
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import Servant.API
import Servant.API.Routes
import Servant.API.Routes.Body
import Servant.API.Routes.Header
import Servant.API.Routes.Param
import Servant.API.Routes.Path
import Servant.API.Routes.Route
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

strTypeRepBody :: Body
strTypeRepBody = oneType @String

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
sameRoutes = getRoutes @l `shouldMatchList` getRoutes @r

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
  describe "Routes" $ do
    describe "makeRoutes/unmakeRoutes" $
      it "works" pending
  describe "HasRoutes" $ do
    describe "base cases" $ do
      it "EmptyAPI" $ getRoutes @EmptyAPI `shouldMatchList` []
      it "UVerb" $ do
        getRoutes @(UVerb 'POST '[JSON] '[])
          `shouldMatchList` [ defRoute "POST"
                            ]
        getRoutes @(UVerb 'POST '[JSON] '[Int])
          `shouldMatchList` [ defRoute "POST"
                                & routeResponseType .~ intTypeRepBody
                            ]
        getRoutes @(UVerb 'POST '[JSON] '[Int, String])
          `shouldMatchList` [ defRoute "POST"
                                & routeResponseType .~ intTypeRepBody <> strTypeRepBody
                            ]
        getRoutes @(UVerb 'POST '[JSON] '[Int, String])
          `shouldMatchList` [ defRoute "POST"
                                & routeResponseType .~ strTypeRepBody <> intTypeRepBody
                            ]
      it "Verb" $ do
        getRoutes @(Post '[JSON] Int) `shouldMatchList` [defRoute "POST" & routeResponseType .~ intTypeRepBody]
        getRoutes @(Post '[JSON] (Headers '[Header "h1" String] Int))
          `shouldMatchList` [ defRoute "POST"
                                & routeResponseType .~ intTypeRepBody
                                & routeResponseHeaders .~ [mkHeaderRep @"h1" @String]
                            ]
      it "Stream" $ do
        getRoutes @(Stream 'POST 201 NoFraming JSON Int) `shouldMatchList` [defRoute "POST" & routeResponseType .~ intTypeRepBody]

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
      it ":<|>" $ getRoutes @(SubAPI :<|> SubAPI2) `shouldMatchList` getRoutes @SubAPI <> getRoutes @SubAPI2
      it "NoContentVerb" $
        showRoute <$> getRoutes @(NoContentVerb 'POST) `shouldMatchList` ["POST /"]
      it "Symbol :>" $ do
        let prep = routePath %~ prependPathPart "sym"
        getRoutes @("sym" :> SubAPI) `shouldMatchList` prep <$> getRoutes @SubAPI
        getRoutes @("sym" :> SubAPI2) `shouldMatchList` prep <$> getRoutes @SubAPI2
        getRoutes @("sym" :> SubAPI3) `shouldMatchList` prep <$> getRoutes @SubAPI3
      it "Header' :>" $ do
        let addH = routeRequestHeaders %~ (mkHeaderRep @"h1" @Int :)
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI) `shouldMatchList` addH <$> getRoutes @SubAPI
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI2) `shouldMatchList` addH <$> getRoutes @SubAPI2
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI3) `shouldMatchList` addH <$> getRoutes @SubAPI3
        let addHOptional = routeRequestHeaders %~ (mkHeaderRep @"h1" @(Maybe Int) :)
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI) `shouldMatchList` addHOptional <$> getRoutes @SubAPI
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI2) `shouldMatchList` addHOptional <$> getRoutes @SubAPI2
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI3) `shouldMatchList` addHOptional <$> getRoutes @SubAPI3
      it "BasicAuth :>" $ do
        let addAuth = routeAuths %~ ("Basic realm" :)
        getRoutes @(BasicAuth "realm" String :> SubAPI) `shouldMatchList` addAuth <$> getRoutes @SubAPI
        getRoutes @(BasicAuth "realm" String :> SubAPI2) `shouldMatchList` addAuth <$> getRoutes @SubAPI2
        getRoutes @(BasicAuth "realm" String :> SubAPI3) `shouldMatchList` addAuth <$> getRoutes @SubAPI3
      it "AuthProtect :>" $ do
        let addAuth = routeAuths %~ ("my-special-auth" :)
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI) `shouldMatchList` addAuth <$> getRoutes @SubAPI
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI2) `shouldMatchList` addAuth <$> getRoutes @SubAPI2
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI3) `shouldMatchList` addAuth <$> getRoutes @SubAPI3
      it "QueryFlag :>" $ do
        let addFlag = routeParams %~ (flagParam @"sym" :)
        getRoutes @(QueryFlag "sym" :> SubAPI) `shouldMatchList` addFlag <$> getRoutes @SubAPI
        getRoutes @(QueryFlag "sym" :> SubAPI2) `shouldMatchList` addFlag <$> getRoutes @SubAPI2
        getRoutes @(QueryFlag "sym" :> SubAPI3) `shouldMatchList` addFlag <$> getRoutes @SubAPI3
      it "QueryParam' :>" $ do
        let addP = routeParams %~ (singleParam @"h1" @Int :)
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI) `shouldMatchList` addP <$> getRoutes @SubAPI
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI2) `shouldMatchList` addP <$> getRoutes @SubAPI2
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI3) `shouldMatchList` addP <$> getRoutes @SubAPI3
        let addPOptional = routeParams %~ (singleParam @"h1" @(Maybe Int) :)
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI) `shouldMatchList` addPOptional <$> getRoutes @SubAPI
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI2) `shouldMatchList` addPOptional <$> getRoutes @SubAPI2
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI3) `shouldMatchList` addPOptional <$> getRoutes @SubAPI3
      it "QueryParams :>" $ do
        let addP = routeParams %~ (arrayElemParam @"h1" @Int :)
        getRoutes @(QueryParams "h1" Int :> SubAPI) `shouldMatchList` addP <$> getRoutes @SubAPI
        getRoutes @(QueryParams "h1" Int :> SubAPI2) `shouldMatchList` addP <$> getRoutes @SubAPI2
        getRoutes @(QueryParams "h1" Int :> SubAPI3) `shouldMatchList` addP <$> getRoutes @SubAPI3
      it "ReqBody' :>" $ do
        let addB = routeRequestBody <>~ intTypeRepBody
        getRoutes @(ReqBody '[JSON] Int :> SubAPI) `shouldMatchList` addB <$> getRoutes @SubAPI
        getRoutes @(ReqBody '[JSON] Int :> SubAPI2) `shouldMatchList` addB <$> getRoutes @SubAPI2
        getRoutes @(ReqBody '[JSON] Int :> SubAPI3) `shouldMatchList` addB <$> getRoutes @SubAPI3
      it "StreamBody' :>" $ do
        let addB = routeRequestBody <>~ intTypeRepBody
        getRoutes @(ReqBody '[JSON] Int :> SubAPI) `shouldMatchList` addB <$> getRoutes @SubAPI
        getRoutes @(ReqBody '[JSON] Int :> SubAPI2) `shouldMatchList` addB <$> getRoutes @SubAPI2
        getRoutes @(StreamBody NoFraming JSON Int :> SubAPI3) `shouldMatchList` addB <$> getRoutes @SubAPI3
      it "Capture' :>" $ do
        let addC = routePath %~ prependCapturePart @Int "cap"
        getRoutes @(Capture "cap" Int :> SubAPI) `shouldMatchList` addC <$> getRoutes @SubAPI
        getRoutes @(Capture "cap" Int :> SubAPI2) `shouldMatchList` addC <$> getRoutes @SubAPI2
        getRoutes @(Capture "cap" Int :> SubAPI3) `shouldMatchList` addC <$> getRoutes @SubAPI3
      it "CaptureAll :>" $ do
        let addC = routePath %~ prependCaptureAllPart @Int "cap"
        getRoutes @(CaptureAll "cap" Int :> SubAPI) `shouldMatchList` addC <$> getRoutes @SubAPI
        getRoutes @(CaptureAll "cap" Int :> SubAPI2) `shouldMatchList` addC <$> getRoutes @SubAPI2
        getRoutes @(CaptureAll "cap" Int :> SubAPI3) `shouldMatchList` addC <$> getRoutes @SubAPI3
#if MIN_VERSION_servant(0,19,0)
      it "NamedRoutes" $
        getRoutes @(NamedRoutes API) `shouldMatchList` getRoutes @SubAPI <> getRoutes @SubAPI2 <> getRoutes @SubAPI3
#endif

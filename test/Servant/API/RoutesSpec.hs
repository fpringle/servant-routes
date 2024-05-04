{-# LANGUAGE CPP #-}

module Servant.API.RoutesSpec
  ( spec
  )
where

import Data.Function
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro
import Servant.API
import Servant.API.Routes
import Servant.API.Routes.Internal.Response
import Servant.API.Routes.Route
import Servant.API.Routes.RouteSpec ()
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q

instance Q.Arbitrary Routes where
  -- we use the 'Routes' pattern to handle removing duplicates etc
  arbitrary = Routes <$> Q.arbitrary
  shrink (Routes routes) = Routes <$> Q.shrink routes

type SubAPI = ReqBody '[JSON] String :> Post '[JSON] Int

type SubAPI2 = Header "h1" T.Text :> "x" :> ("y" :> Put '[JSON] String :<|> SubAPI)

type SubAPI3 =
  Header "h1" T.Text
    :> "x"
    :> ( "y" :> Put '[JSON] String
          :<|> "z" :> Header' '[Optional] "h2" Int :> Get '[JSON] [Integer]
       )

intRequest :: Request
intRequest = oneRequest @Int

intResponse :: Responses
intResponse = oneResponse @Int

strResponse :: Responses
strResponse = oneResponse @String

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
      prop "correctly removes duplicates" $
        \routes ->
          let -- ~ routesList = unmakeRoutes routes
              Routes routesList = routes
              -- ~ routes2 = unmakeRoutes (routesList <> routesList)
              routes2 = Routes (routesList <> routesList)
          in  routes2 === routes
  describe "HasRoutes" $ do
    describe "base cases" $ do
      it "EmptyAPI" $ getRoutes @EmptyAPI `shouldMatchList` []
      it "UVerb" $ do
        getRoutes @(UVerb 'POST '[JSON] '[])
          `shouldMatchList` [ defRoute "POST"
                            ]
        getRoutes @(UVerb 'POST '[JSON] '[Int])
          `shouldMatchList` [ defRoute "POST"
                                & routeResponse .~ intResponse
                            ]
        getRoutes @(UVerb 'POST '[JSON] '[Int, String])
          `shouldMatchList` [ defRoute "POST"
                                & routeResponse .~ intResponse <> strResponse
                            ]
        getRoutes @(UVerb 'POST '[JSON] '[Headers '[] Int, String])
          `shouldMatchList` [ defRoute "POST"
                                & routeResponse .~ strResponse <> intResponse
                            ]
        getRoutes
          @( UVerb
              'POST
              '[JSON]
              '[ Headers '[] (Headers '[Header "h2" String] Int)
               , Headers '[Header "h1" [Int], Header "h3" Int] String
               ]
           )
          `shouldMatchList` [ defRoute "POST"
                                & routeResponse
                                  .~ ( strResponse
                                        & unResponses . traversed . responseHeaders
                                          <>~ Set.fromList
                                            [ mkHeaderRep @"h1" @[Int]
                                            , mkHeaderRep @"h3" @Int
                                            ]
                                     )
                                    <> ( intResponse
                                          & (unResponses . traversed . responseHeaders)
                                            `add` (mkHeaderRep @"h2" @String)
                                       )
                            ]
      it "Verb" $ do
        getRoutes @(Post '[JSON] Int) `shouldMatchList` [defRoute "POST" & routeResponse .~ intResponse]
        getRoutes @(Post '[JSON] (Headers '[Header "h1" String] Int))
          `shouldMatchList` [ defRoute "POST"
                                & routeResponse .~ oneResponse @(Headers '[Header "h1" String] Int)
                            ]
      it "Stream" $ do
        getRoutes @(Stream 'POST 201 NoFraming JSON Int) `shouldMatchList` [defRoute "POST" & routeResponse .~ intResponse]

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
        renderRoute <$> getRoutes @(NoContentVerb 'POST) `shouldMatchList` ["POST /"]
      it "Symbol :>" $ do
        let prep = routePath %~ prependPathPart "sym"
        getRoutes @("sym" :> SubAPI) `shouldMatchList` prep <$> getRoutes @SubAPI
        getRoutes @("sym" :> SubAPI2) `shouldMatchList` prep <$> getRoutes @SubAPI2
        getRoutes @("sym" :> SubAPI3) `shouldMatchList` prep <$> getRoutes @SubAPI3
      it "Header' :>" $ do
        let addH = routeRequestHeaders `add` (mkHeaderRep @"h1" @Int)
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI) `shouldMatchList` addH <$> getRoutes @SubAPI
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI2) `shouldMatchList` addH <$> getRoutes @SubAPI2
        getRoutes @(Header' '[Required] "h1" Int :> SubAPI3) `shouldMatchList` addH <$> getRoutes @SubAPI3
        let addHOptional = routeRequestHeaders `add` (mkHeaderRep @"h1" @(Maybe Int))
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI) `shouldMatchList` addHOptional <$> getRoutes @SubAPI
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI2) `shouldMatchList` addHOptional <$> getRoutes @SubAPI2
        getRoutes @(Header' '[Optional] "h1" Int :> SubAPI3) `shouldMatchList` addHOptional <$> getRoutes @SubAPI3
      it "BasicAuth :>" $ do
        let addAuth = routeAuths `add` basicAuth @"realm"
        getRoutes @(BasicAuth "realm" String :> SubAPI) `shouldMatchList` addAuth <$> getRoutes @SubAPI
        getRoutes @(BasicAuth "realm" String :> SubAPI2) `shouldMatchList` addAuth <$> getRoutes @SubAPI2
        getRoutes @(BasicAuth "realm" String :> SubAPI3) `shouldMatchList` addAuth <$> getRoutes @SubAPI3
      it "AuthProtect :>" $ do
        let addAuth = routeAuths `add` customAuth @"my-special-auth"
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI) `shouldMatchList` addAuth <$> getRoutes @SubAPI
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI2) `shouldMatchList` addAuth <$> getRoutes @SubAPI2
        getRoutes @(AuthProtect "my-special-auth" :> SubAPI3) `shouldMatchList` addAuth <$> getRoutes @SubAPI3
      it "QueryFlag :>" $ do
        let addFlag = routeParams `add` (flagParam @"sym")
        getRoutes @(QueryFlag "sym" :> SubAPI) `shouldMatchList` addFlag <$> getRoutes @SubAPI
        getRoutes @(QueryFlag "sym" :> SubAPI2) `shouldMatchList` addFlag <$> getRoutes @SubAPI2
        getRoutes @(QueryFlag "sym" :> SubAPI3) `shouldMatchList` addFlag <$> getRoutes @SubAPI3
      it "QueryParam' :>" $ do
        let addP = routeParams `add` (singleParam @"h1" @Int)
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI) `shouldMatchList` addP <$> getRoutes @SubAPI
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI2) `shouldMatchList` addP <$> getRoutes @SubAPI2
        getRoutes @(QueryParam' '[Required] "h1" Int :> SubAPI3) `shouldMatchList` addP <$> getRoutes @SubAPI3
        let addPOptional = routeParams `add` (singleParam @"h1" @(Maybe Int))
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI) `shouldMatchList` addPOptional <$> getRoutes @SubAPI
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI2) `shouldMatchList` addPOptional <$> getRoutes @SubAPI2
        getRoutes @(QueryParam' '[Optional] "h1" Int :> SubAPI3) `shouldMatchList` addPOptional <$> getRoutes @SubAPI3
      it "QueryParams :>" $ do
        let addP = routeParams `add` (arrayElemParam @"h1" @Int)
        getRoutes @(QueryParams "h1" Int :> SubAPI) `shouldMatchList` addP <$> getRoutes @SubAPI
        getRoutes @(QueryParams "h1" Int :> SubAPI2) `shouldMatchList` addP <$> getRoutes @SubAPI2
        getRoutes @(QueryParams "h1" Int :> SubAPI3) `shouldMatchList` addP <$> getRoutes @SubAPI3
      it "ReqBody' :>" $ do
        let addB = routeRequestBody <>~ intRequest
        getRoutes @(ReqBody '[JSON] Int :> SubAPI) `shouldMatchList` addB <$> getRoutes @SubAPI
        getRoutes @(ReqBody '[JSON] Int :> SubAPI2) `shouldMatchList` addB <$> getRoutes @SubAPI2
        getRoutes @(ReqBody '[JSON] Int :> SubAPI3) `shouldMatchList` addB <$> getRoutes @SubAPI3
      it "StreamBody' :>" $ do
        let addB = routeRequestBody <>~ intRequest
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

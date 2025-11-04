{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Servant.API.Routes.GoldenSpec
  ( spec
  )
where

import Data.ByteString
import qualified Data.Text as T
import GHC.Generics
import Servant.API
#if MIN_VERSION_servant(0,20,3)
import Servant.API.MultiVerb
#endif
import Servant.API.Routes.Golden
import Test.Hspec as H

type SubAPI = ReqBody '[JSON] String :> Post '[JSON] Int

type SubAPI2 = Header "h1" T.Text :> "x" :> ("y" :> Put '[JSON] String :<|> SubAPI)

type SubAPI3 =
  Header "h2" T.Text
    :> "x2"
    :> ( "y1" :> Description "desc" :> Put '[JSON] String
          :<|> "z1" :> Header' '[Optional] "h3" Int :> Get '[JSON] [Integer]
       )

#if MIN_VERSION_servant(0,19,0)
data API mode = API
  { ep1 :: mode :- "sub1" :> SubAPI
  , ep2 :: mode :- "sub2" :> SubAPI2
  , ep3 :: mode :- "sub3" :> SubAPI3
  } deriving Generic
#endif

#if MIN_VERSION_servant(0,20,3)
data MultiAPI mode = MultiAPI
  { multi1 :: mode :- "multi1" :> MultiVerb 'POST '[JSON] '[] (Union '[])
  , multi2 :: mode :- "multi2" :> MultiVerb 'POST '[JSON] '[Respond 200 "hello" Int] Int
  , multi3 :: mode :- "multi3" :> MultiVerb 'POST '[JSON] '[RespondAs '[JSON] 200 "int" Int] Int
  , multi4 :: mode :- "multi4" :> MultiVerb 'POST '[JSON] '[RespondAs '[JSON] 200 "int" Int, RespondStreaming 200 "string-streaming" () String] (Union '[Int, SourceIO ByteString])
  } deriving Generic
#endif

spec :: H.Spec
spec = do
  it "SubAPI" $ goldenRoutes @SubAPI (show ''SubAPI)
  it "SubAPI2" $ goldenRoutes @SubAPI2 (show ''SubAPI2)
  it "SubAPI3" $ goldenRoutes @SubAPI3 (show ''SubAPI3)
#if MIN_VERSION_servant(0,19,0)
  it "NamedRoutes API" $ goldenRoutes @(NamedRoutes API) (show ''API)
#endif
#if MIN_VERSION_servant(0,20,3)
  it "MultiVerb API" $ goldenRoutes @(NamedRoutes MultiAPI) (show ''MultiAPI)
#endif

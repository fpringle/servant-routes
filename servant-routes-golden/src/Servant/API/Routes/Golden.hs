{- |
Module      : Servant.API.Routes.Golden
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com
-}
module Servant.API.Routes.Golden
  ( goldenRoutes
  , goldenRoutesSpec
  )
where

import Control.Monad ((>=>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TL
import Servant.API.Routes
import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Golden as G

goldenRoutes :: forall api. (HasRoutes api) => String -> G.Golden A.Value
goldenRoutes name =
  (G.defaultGolden name "")
    { G.output = A.toJSON . Routes $ getRoutes @api
    , G.encodePretty = T.unpack . TL.toStrict . pretty
    , G.writeToFile = \fp -> TL.writeFile fp . pretty
    , G.readFromFile = A.eitherDecodeFileStrict @A.Value >=> either fail pure
    }
  where
    pretty = TLE.decodeUtf8 . P.encodePretty' (P.defConfig {P.confCompare = compare})

goldenRoutesSpec :: forall api. (HasRoutes api) => String -> H.Spec
goldenRoutesSpec = H.it "Generates the correct Routes" . goldenRoutes @api

{- |
Module      : Servant.API.Routes.Golden
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

The 'HasRoutes' class allows us to generate a list of 'Route's from a Servant
API type. Using "hspec-golden", we can generate automatic 'G.Golden' tests from
these APIs. If such a test fails, we know that the shape of our API has changed.
Therefore we must either:

- decide that the shape change is correct, and acknowledge that the golden files
  should be updated, by running the
  [hgold CLI](https://github.com/stackbuilders/hspec-golden?tab=readme-ov-file#install-cli), or
- realise that our changes resulted in a change to the API which we didn't
  intend/anticipate, so we have to fix them.
-}
module Servant.API.Routes.Golden
  ( -- * Generating golden tests using HasRoutes
    goldenRoutes
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

{- | Given an API type with a 'HasRoutes' instance, we can create a 'G.Golden' test
on the t'Routes' representation of that API. This can be used to automatically track
changes to the API shape during testing. For a concrete example see the
[README](https://github.com/fpringle/servant-routes/blob/main/servant-routes-golden/README.md).
-}
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

-- | Generate a 'H.Spec' for your API type.
goldenRoutesSpec :: forall api. (HasRoutes api) => String -> H.Spec
goldenRoutesSpec = H.it "Generates the correct Routes" . goldenRoutes @api

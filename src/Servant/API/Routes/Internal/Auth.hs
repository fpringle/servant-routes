{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Auth
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Auth
  ( Auth (..)
  )
where

import Data.Aeson
import qualified Data.Text as T

{- | There are 2 variants:

- \"Basic" authentication: corresponds to the 'Servant.API.BasicAuth' type. Construct with 'Servant.API.Routes.Auth.basicAuth'.
- \"Custom" authentication: corresponds to the 'Servant.API.AuthProtect' type. Construct with 'Servant.API.Routes.Auth.customAuth'.
-}
data Auth
  = Basic T.Text
  | Custom T.Text
  deriving (Show, Eq, Ord)

instance ToJSON Auth where
  toJSON =
    toJSON @T.Text . \case
      Basic realm -> "Basic " <> realm
      Custom tag -> tag

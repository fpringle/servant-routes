{- |
Module      : Servant.API.Routes.Internal.Description
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Description
  ( ResponseDescription (..)
  )
where

import Data.Aeson
import Data.String (IsString)
import qualified Data.Text as T

{- | Description of a route response. This will correspond to the Servant @Description@ combinator.

It should  be noted that the 'HasRoutes' behaviour for @Description@ diverges from that in
@servant-openapi3@, in the case that one EP has multiple @Description@ combinators.
For example, given the following API:

@
type MyAPI =
  "transaction" :> TransactionAPI
    :\<|> "user" :> Description "User sub-API"
          :> ( Description "Get my user" :> Get '[JSON] User
              :\<|> "list" :> Get '[JSON] [User]
             )
@

The @Operation@ that @servant-openapi@ generates for the @GET /user@ endpoint will have the two
@Description@s 'mappend'-ed together: @"User sub-APIGet my user"@.

The corresponding 'Route' will take the most specific 'ResponseDescription': @"Get my user"@.
-}
newtype ResponseDescription = ResponseDescription {unDescription :: T.Text}
  deriving (Show)
  deriving (Eq, IsString, Ord, Semigroup, Monoid, ToJSON, FromJSON) via T.Text

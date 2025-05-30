{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Servant.API.Routes.Internal.Route
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Route
  ( -- * API routes
    Route (..)
  , RouteDescription (..)
  , RouteSummary (..)

    -- * Optics #optics#
  , routeMethod
  , routePath
  , routeParams
  , routeRequestHeaders
  , routeRequestBody
  , routeResponse
  , routeAuths
  , routeDescription
  , routeSummary
  )
where

import Data.Aeson
import Data.Function (on)
import qualified Data.Set as Set
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lens.Micro.TH
import Network.HTTP.Types.Method (Method)
import "this" Servant.API.Routes.Auth
import "this" Servant.API.Routes.Header
import "this" Servant.API.Routes.Internal.Request
import "this" Servant.API.Routes.Internal.Response
import "this" Servant.API.Routes.Param
import "this" Servant.API.Routes.Path

{- | Description of a route. This will correspond to the Servant @Description@ combinator.

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

The corresponding 'Route' will take the most specific 'RouteDescription': @"Get my user"@.
-}
newtype RouteDescription = RouteDescription {unDescription :: T.Text}
  deriving (Show)
  deriving (Eq, IsString, Ord, Semigroup, Monoid, ToJSON, FromJSON) via T.Text

{- | Short summary of a route. This will correspond to the Servant @Summary@ combinator.
The behaviour described for 'RouteDescription' is the same for 'RouteSummary'.
-}
newtype RouteSummary = RouteSummary {unSummary :: T.Text}
  deriving (Show)
  deriving (Eq, IsString, Ord, Semigroup, Monoid, ToJSON, FromJSON) via T.Text

-- | A simple representation of a single endpoint of an API.
data Route = Route
  { _routeMethod :: Method
  , _routePath :: Path
  , _routeParams :: Set.Set Param
  , _routeRequestHeaders :: Set.Set HeaderRep
  , _routeRequestBody :: Request
  , _routeResponse :: Responses
  , _routeAuths :: Set.Set Auth
  , _routeDescription :: Maybe RouteDescription
  , _routeSummary :: Maybe RouteSummary
  }
  deriving (Show, Eq)

makeLenses ''Route

instance Ord Route where
  compare = compare `on` \Route {..} -> (_routePath, _routeMethod)

instance ToJSON Route where
  toJSON Route {..} =
    object
      [ "method" .= TE.decodeUtf8 _routeMethod
      , "path" .= _routePath
      , "params" .= _routeParams
      , "request_headers" .= _routeRequestHeaders
      , "request_body" .= _routeRequestBody
      , "response" .= _routeResponse
      , "auths" .= _routeAuths
      , "description" .= _routeDescription
      , "summary" .= _routeSummary
      ]

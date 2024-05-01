{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Servant.API.Routes.Internal.Route
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Route
  ( -- * API routes
    Route (..)

    -- * Optics #optics#
  , routeMethod
  , routePath
  , routeParams
  , routeRequestHeaders
  , routeRequestBody
  , routeResponseHeaders
  , routeResponseType
  , routeAuths
  )
where

import Data.Aeson
import qualified Data.Aeson.Key as AK (fromText)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lens.Micro.TH
import Network.HTTP.Types.Method (Method)
import "this" Servant.API.Routes.Header
import "this" Servant.API.Routes.Internal.Body
import "this" Servant.API.Routes.Param
import "this" Servant.API.Routes.Path
import "this" Servant.API.Routes.Utils

{- | A simple representation of a single endpoint of an API.

The 'Route' type is not sophisticated, and its internals are hidden.
Create 'Route's using 'defRoute', and update its fields using the provided [lenses](#g:optics).
-}
data Route = Route
  { _routeMethod :: Method
  , _routePath :: Path
  , _routeParams :: [Param]
  , _routeRequestHeaders :: [HeaderRep]
  , _routeRequestBody :: Body
  , _routeResponseHeaders :: [HeaderRep]
  , _routeResponseType :: Body
  , _routeAuths :: [T.Text]
  }
  deriving (Show, Eq)

makeLenses ''Route

instance Ord Route where
  compare = compare `on` \Route {..} -> (_routePath, _routeMethod)

bodyToJSONAs :: T.Text -> Body -> Value
bodyToJSONAs lbl = \case
  NoBody -> Null
  OneType tRep -> typeRepToJSON tRep
  ManyTypes tReps ->
    object [AK.fromText lbl .= fmap typeRepToJSON tReps]

instance ToJSON Route where
  toJSON Route {..} =
    object
      [ "method" .= TE.decodeUtf8 _routeMethod
      , "path" .= _routePath
      , "params" .= _routeParams
      , "request_headers" .= _routeRequestHeaders
      , "request_body" .= bodyToJSONAs "all_of" _routeRequestBody
      , "response_headers" .= _routeResponseHeaders
      , "response" .= bodyToJSONAs "one_of" _routeResponseType
      , "auths" .= _routeAuths
      ]

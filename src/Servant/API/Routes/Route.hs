{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Servant.API.Routes.Route
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Simple term-level representation of Servant API endpoints.
-}
module Servant.API.Routes.Route
  ( -- * API routes
    Route
  , defRoute
  , showRoute

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

{- | Given a REST 'Method', create a default 'Route': root path (@"/"@) with no params,
headers, body, auths, or response.
-}
defRoute :: Method -> Route
defRoute method =
  Route
    { _routeMethod = method
    , _routePath = rootPath
    , _routeParams = mempty
    , _routeRequestHeaders = mempty
    , _routeRequestBody = mempty
    , _routeResponseHeaders = mempty
    , _routeResponseType = mempty
    , _routeAuths = mempty
    }

{- | Pretty-print a 'Route'. Note that the output is minimal and doesn't contain all the information
contained in a 'Route'. For full output, use the 'ToJSON' instance.

> ghci> showRoute $ defRoute \"POST\"
> "POST /"
> ghci> :{
> ghci| showRoute $
> ghci|   defRoute \"POST\"
> ghci|     & routePath %~ prependPathPart "api/v2"
> ghci|     & routeParams .~ [singleParam @"p1" @T.Text, flagParam @"flag", arrayElemParam @"p2s" @(Maybe Int)]
> ghci| :}
> "POST /api/v2?p1=<Text>&flag&p2s=<[Maybe Int]>"
-}
showRoute :: Route -> T.Text
showRoute Route {..} =
  mconcat
    [ method
    , " "
    , path
    , params
    ]
  where
    method = TE.decodeUtf8 _routeMethod
    path = renderPath _routePath
    params =
      if null _routeParams
        then ""
        else "?" <> T.intercalate "&" (renderParam <$> _routeParams)

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

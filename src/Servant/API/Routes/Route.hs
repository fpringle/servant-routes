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
  , routeResponse
  , routeAuths
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Method (Method)
import "this" Servant.API.Routes.Internal.Route
import "this" Servant.API.Routes.Param
import "this" Servant.API.Routes.Path
import "this" Servant.API.Routes.Request
import "this" Servant.API.Routes.Response

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
    , _routeRequestBody = noRequest
    , _routeResponse = noResponse
    , _routeAuths = mempty
    }

{- | Pretty-print a 'Route'. Note that the output is minimal and doesn't contain all the information
contained in a 'Route'. For full output, use the 'Data.Aeson.ToJSON' instance.

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

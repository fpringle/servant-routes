{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Servant.API.Routes
Description: Simple typeclass to get all the routes of an API.
-}
module Servant.API.Routes
  ( -- * API routes
    Route
  , showRoute
  , Routes
  , unRoutes
  , pattern Routes

    -- * Automatic generation of routes for Servant API types
  , HasRoutes (..)
  , printRoutes

    -- * Optics
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
import qualified Data.Aeson.Types as A (Pair)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', traverse_)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol)
import Lens.Micro
import Lens.Micro.TH
import Network.HTTP.Types.Method (Method)
import Servant.API
import Servant.API.Modifiers (RequiredArgument)
import "this" Servant.API.Routes.Header
import "this" Servant.API.Routes.Param
import "this" Servant.API.Routes.Path
import "this" Servant.API.Routes.Utils

-- import Servant.Links hiding (Param)

-- | A simple representation of a single endpoint of an API.
data Route = Route
  { _routeMethod :: Method
  , _routePath :: Path
  , _routeParams :: [Param]
  , _routeRequestHeaders :: [HeaderRep]
  , _routeRequestBody :: Maybe TypeRep
  , _routeResponseHeaders :: [HeaderRep]
  , _routeResponseType :: Maybe TypeRep
  , _routeAuths :: [T.Text]
  }
  deriving (Show, Eq)

makeLenses ''Route

defRoute :: Method -> Route
defRoute method =
  Route
    { _routeMethod = method
    , _routePath = rootPath
    , _routeParams = mempty
    , _routeRequestHeaders = mempty
    , _routeRequestBody = Nothing
    , _routeResponseHeaders = mempty
    , _routeResponseType = Nothing
    , _routeAuths = mempty
    }

{- | To render all of an API's 'Route's as JSON, we need to identify each route by its
 path AND its method (since 2 routes can have the same path but different method).
 This newtype lets us represent this nested structure.
-}
newtype Routes = UnsafeRoutes
  { unRoutes :: Map.Map Path (Map.Map Method Route)
  }
  deriving (Show, Eq)

makeRoutes :: [Route] -> Routes
makeRoutes = UnsafeRoutes . foldl' insert mempty
  where
    insert acc r = Map.insertWith (<>) path subMap acc
      where
        path = _routePath r
        method = _routeMethod r
        subMap = Map.singleton method r

unmakeRoutes :: Routes -> [Route]
unmakeRoutes = concatMap Map.elems . Map.elems . unRoutes

pattern Routes :: [Route] -> Routes
pattern Routes rs <- (unmakeRoutes -> rs)
  where
    Routes = makeRoutes

{-# COMPLETE Routes #-}

instance ToJSON Route where
  toJSON Route {..} =
    object
      [ "method" .= TE.decodeUtf8 _routeMethod
      , "path" .= _routePath
      , "params" .= _routeParams
      , "request_headers" .= _routeRequestHeaders
      , "request_body" .= (typeRepToJSON <$> _routeRequestBody)
      , "response_headers" .= _routeResponseHeaders
      , "response" .= (typeRepToJSON <$> _routeResponseType)
      , "auths" .= _routeAuths
      ]

instance ToJSON Routes where
  toJSON = object . fmap mkPair . Map.assocs . unRoutes
    where
      mkPair :: (Path, Map.Map Method Route) -> A.Pair
      mkPair = bimap (AK.fromText . renderPath) subMapToJSON

      subMapToJSON :: Map.Map Method Route -> Value
      subMapToJSON = object . fmap mkSubPair . Map.assocs

      mkSubPair (method, r) =
        let key = AK.fromText . TE.decodeUtf8 $ method
        in  key .= r

{- | Get a simple list of all the routes in an API.

Refactoring Servant API types is quite
error-prone, especially when you have to move around lots of ':<|>' and ':>'.
So it's very possible that the route structure could change in that
refactoring without being caught by the type-checker.

The 'HasRoutes' class (which is basically a very very simple version of HasOpenApi
from [servant-openapi3](https://hackage.haskell.org/package/servant-openapi3))
could help as a sanity check - run 'printRoutes' before and after the refactor,
and if they give the same output then things are probably fine.

Note that 'printRoutes' only includes the path, method and query parameters.
For more detailed comparison, use the JSON instance of `Routes`, encode the routes to
a file (before and after the refactoring), and use jdiff.
-}
class HasRoutes api where
  getRoutes :: [Route]

printRoutes :: forall api. HasRoutes api => IO ()
printRoutes = traverse_ printRoute $ getRoutes @api
  where
    printRoute = T.putStrLn . showRoute

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

instance HasRoutes EmptyAPI where
  getRoutes = mempty

instance
  ReflectMethod (method :: StdMethod) =>
  HasRoutes (NoContentVerb method)
  where
  getRoutes = pure $ defRoute method
    where
      method = reflectMethod $ Proxy @method

instance
  {-# OVERLAPPABLE #-}
  (ReflectMethod (method :: StdMethod), Typeable a) =>
  HasRoutes (Verb method status ctypes a)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponseType ?~ response
    where
      method = reflectMethod $ Proxy @method
      response = typeRep $ Proxy @a

instance
  {-# OVERLAPPING #-}
  ( ReflectMethod (method :: StdMethod)
  , GetHeaderReps hs
  , Typeable a
  ) =>
  HasRoutes (Verb method status ctypes (Headers hs a))
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponseHeaders .~ headers
        & routeResponseType ?~ response
    where
      method = reflectMethod $ Proxy @method
      headers = getHeaderReps @hs
      response = typeRep $ Proxy @a

instance
  (ReflectMethod (method :: StdMethod), Typeable as) =>
  HasRoutes (UVerb method ctypes as)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponseType ?~ response
    where
      method = reflectMethod $ Proxy @method
      response = typeRep $ Proxy @as

instance (HasRoutes l, HasRoutes r) => HasRoutes (l :<|> r) where
  getRoutes = getRoutes @l <> getRoutes @r

instance (KnownSymbol path, HasRoutes api) => HasRoutes (path :> api) where
  getRoutes = getRoutes @api <&> routePath %~ prependPathPart path
    where
      path = knownSymbolT @path

instance
  (Typeable a, HasRoutes api) =>
  HasRoutes (Capture' mods capture a :> api)
  where
  getRoutes = getRoutes @api <&> routePath %~ prependPathPart capture
    where
      capture = "<" <> showTypeRep @a <> ">"

instance
  (Typeable [a], HasRoutes api) =>
  HasRoutes (CaptureAll capture a :> api)
  where
  getRoutes = getRoutes @api <&> routePath %~ prependPathPart capture
    where
      capture = "<" <> showTypeRep @[a] <> ">"

instance
  (KnownSymbol sym, Typeable (RequiredArgument mods a), HasRoutes api) =>
  HasRoutes (QueryParam' mods sym a :> api)
  where
  getRoutes = getRoutes @api <&> routeParams %~ (param :)
    where
      param = singleParam @sym @(RequiredArgument mods a)

instance
  (KnownSymbol sym, Typeable a, HasRoutes api) =>
  HasRoutes (QueryParams sym a :> api)
  where
  getRoutes = getRoutes @api <&> routeParams %~ (param :)
    where
      param = arrayElemParam @sym @a

instance (HasRoutes (ToServantApi routes)) => HasRoutes (NamedRoutes routes) where
  getRoutes = getRoutes @(ToServantApi routes)

instance (KnownSymbol sym, HasRoutes api) => HasRoutes (QueryFlag sym :> api) where
  getRoutes = getRoutes @api <&> routeParams %~ (param :)
    where
      param = flagParam @sym

instance (HasRoutes api, Typeable a) => HasRoutes (ReqBody' mods list a :> api) where
  getRoutes = getRoutes @api <&> routeRequestBody ?~ reqBody
    where
      reqBody = typeRep $ Proxy @a

instance (HasRoutes api) => HasRoutes (Vault :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (HttpVersion :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api, KnownSymbol realm) => HasRoutes (BasicAuth realm usr :> api) where
  getRoutes = getRoutes @api <&> routeAuths %~ (auth :)
    where
      auth = "Basic " <> knownSymbolT @realm

instance (HasRoutes api) => HasRoutes (Description :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (Summary :> api) where
  getRoutes = getRoutes @api

instance
  (HasRoutes api, KnownSymbol tag) =>
  HasRoutes (AuthProtect (tag :: Symbol) :> api)
  where
  getRoutes = getRoutes @api <&> routeAuths %~ (auth :)
    where
      auth = knownSymbolT @tag

instance
  (HasRoutes api, KnownSymbol sym, Typeable (RequiredArgument mods a)) =>
  HasRoutes (Header' mods sym a :> api)
  where
  getRoutes = getRoutes @api <&> routeRequestHeaders %~ (header :)
    where
      header = mkHeaderRep @sym @(RequiredArgument mods a)

instance (HasRoutes api) => HasRoutes (Fragment :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (IsSecure :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (RemoteHost :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (StreamBody' mods framing ct a :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (WithNamedContext name subContext api) where
  getRoutes = getRoutes @api

instance
  (ReflectMethod (method :: StdMethod), Typeable a) =>
  HasRoutes (Stream method status framing ctype a)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponseType ?~ response
    where
      method = reflectMethod $ Proxy @method
      response = typeRep $ Proxy @a

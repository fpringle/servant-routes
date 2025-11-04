{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Servant.API.Routes
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

This package provides two things:

  1. A simple, and probably incomplete, way to represent APIs at the term level.
     This is achieved by the 'Route', t'Routes', 'Path', 'Param', 'HeaderRep',
     'Request', 'Response' and 'Responses' types.
  2. More interestingly, a way to automatically generate the routes from any Servant API.  This is
     accomplished using the 'HasRoutes' typeclass. You can think of this as being a less sophisticated
     version of @HasOpenApi@ from [servant-openapi3](https://hackage.haskell.org/package/servant-openapi3),
     or a more sophisticated version of @layout@ from
     [servant-server](https://hackage.haskell.org/package/servant-server).

= Motivation

Refactoring Servant API types is quite error-prone, especially when you have to move
around lots of ':<|>' and ':>'.  So it's very possible that the route structure could
change in that refactoring, /without being caught by the type-checker/.

The 'HasRoutes' class could help as a golden test - run 'getRoutes' before and after
the refactor, and if they give the same result you can be much more confident that the
refactor didn't introduce difficult bugs.

- Note that 'printRoutes' only includes the path, method and query parameters.
  For more detailed comparison, use the JSON instance of t'Routes', encode the routes to
  a file (before and after the refactoring), and use [jdiff](https://github.com/networktocode/jdiff).

Another use-case is in testing: some Haskellers use type families to modify Servant APIs, for example
to add endpoints or authorisation headers. Types are hard to test, but terms are easy. Use 'HasRoutes'
and run your tests on t'Routes'.
-}
module Servant.API.Routes
  ( -- * API routes
    Route
  , defRoute
  , renderRoute
  , Routes
  , unRoutes
  , pattern Routes

    -- * Automatic generation of routes for Servant API types

    -- | Now we can automatically generate a t'Routes' for any Servant combinator. In most cases
    -- the user should never need to implement 'HasRoutes' unless they're hacking on Servant or
    -- defining their own combinators.
  , HasRoutes (..)
  , printRoutes
  , printRoutesSorted
  , printRoutesJSON
  , printRoutesJSONPretty

    -- * Types and helper functions

    -- ** URL paths
  , Path
  , rootPath
  , prependPathPart
  , prependCapturePart
  , prependCaptureAllPart
  , renderPath

    -- ** Request/response bodies

    -- *** Requests
  , Request
  , noRequest
  , oneRequest
  , allOfRequests

    -- *** Responses
  , Response
  , responseType
  , responseHeaders
  , Responses
  , noResponse
  , oneResponse
  , oneOfResponses

    -- ** Request/response headers
  , HeaderRep
  , mkHeaderRep

    -- ** Query parameters
  , Param
  , singleParam
  , arrayElemParam
  , flagParam
  , renderParam

    -- ** Auth schemes
  , Auth
  , basicAuth
  , customAuth
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Key as AK (fromText)
import qualified Data.Aeson.Types as A (Pair)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', traverse_)
import Data.Kind (Type)
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol)
import Lens.Micro
import Network.HTTP.Types.Method (Method)
import Servant.API
import Servant.API.Modifiers (RequiredArgument)
#if MIN_VERSION_servant(0,20,3)
import Servant.API.MultiVerb
#endif
import "this" Servant.API.Routes.Auth
import "this" Servant.API.Routes.Header
import "this" Servant.API.Routes.Param
import "this" Servant.API.Routes.Path
import "this" Servant.API.Routes.Request
import "this" Servant.API.Routes.Response
import "this" Servant.API.Routes.Route
import "this" Servant.API.Routes.Utils

{- | To render all of an API's 'Route's as JSON, we need to identify each route by its
 path AND its method (since 2 routes can have the same path but different method).
 This newtype lets us represent this nested structure.
-}
newtype Routes = MkRoutes
  { unRoutes :: Map.Map Path (Map.Map Method Route)
  -- ^ Get the underlying 'Map.Map' of a t'Routes'.
  }
  deriving (Show, Eq)

makeRoutes :: [Route] -> Routes
makeRoutes = MkRoutes . foldl' insert mempty
  where
    insert acc r = Map.insertWith (<>) path subMap acc
      where
        path = r ^. routePath
        method = r ^. routeMethod
        subMap = Map.singleton method r

unmakeRoutes :: Routes -> [Route]
unmakeRoutes = concatMap Map.elems . Map.elems . unRoutes

{- | A smart constructor that allows us to think of a t'Routes' as simply a list of
'Route's, whereas it's actually a 'Map.Map'.
-}
pattern Routes :: [Route] -> Routes
pattern Routes rs <- (unmakeRoutes -> rs)
  where
    Routes = makeRoutes

{-# COMPLETE Routes #-}

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

One use-case, which was the original motivation for the class, is refactoring Servant APIs
to use 'NamedRoutes'. It's a fiddly, repetitive, and error-prone process, and it's very
easy to make small mistakes. A lot of these errors will be caught by the type-checker, e.g.
if the type signature of a handler function doesn't match the @ServerT@ of its API type.
However there are some errrors that wouldn't be caught by the type-checker, such as missing
out path parts.

For example, if our original API looked like

> type API =
>   "api"
>     :> "v2"
>     :> ( "users" :> Get '[JSON] [User]
>           :<|> "user" :> ReqBody '[JSON] UserData :> Post '[JSON] UserId
>        )
>
> server :: Server API
> server = listUsers :<|> createUser
>   where ...

and we refactored to

> data RefactoredAPI mode = RefactoredAPI
>   { listUsers :: mode :- "api" :> "v2" :> "users" :> Get '[JSON] [User]
>   , createUser :: mode :- "user" :> ReqBody '[JSON] UserData :> Post '[JSON] UserId
>   }
>   deriving Generic
>
> server :: Server (NamedRoutes RefactoredAPI)
> server = RefactoredAPI {listUsers, createUser}
>   where ...

Oops! We forgot the @"api" :> "v2" :>@ in the 2nd sub-endpoint. However, since the @ServerT@ type
is unaffected by adding or remove path parts, this will still compile.

However, if we use 'HasRoutes' as a sanity check:

> ghci> printRoutes @API
> GET /api/v2/users
> POST /api/v2/user
>
> ghci> printRoutes @(NamedRoutes RefactoredAPI)
> GET /api/v2/users
> POST /user

Much clearer to see the mistake. For more detailed output, use the 'ToJSON' instance:

> ghci> BL.putStrLn . encodePretty $ getRoutes @API
> [
>     {
>         "auths": [],
>         "method": "GET",
>         "params": [],
>         "path": "/api/v2/users",
>         "request_body": null,
>         "request_headers": [],
>         "response": {
>             "description": null,
>             "headers": [],
>             "type": "[User]"
>         },
>         "summary": null
>     },
>     {
>         "auths": [],
>         "method": "POST",
>         "params": [],
>         "path": "/api/v2/user",
>         "request_body": "UserData",
>         "request_headers": [],
>         "response": {
>             "description": null,
>             "headers": [],
>             "type": "UserId"
>         },
>         "summary": null
>     }
> ]
>
> ghci> BL.putStrLn . encodePretty $ getRoutes @(NamedRoutes RefactoredAPI)
> [
>     {
>         "auths": [],
>         "method": "GET",
>         "params": [],
>         "path": "/api/v2/users",
>         "request_body": null,
>         "request_headers": [],
>         "response": {
>             "description": null,
>             "headers": [],
>             "type": "[User]"
>         },
>         "summary": null
>     },
>     {
>         "auths": [],
>         "method": "POST",
>         "params": [],
>         "path": "/user",              -- oops!
>         "request_body": "UserData",
>         "request_headers": [],
>         "response": {
>             "description": null,
>             "headers": [],
>             "type": "UserId"
>         },
>         "summary": null
>     }
> ]
-}
class HasRoutes api where
  -- | Type-level list of API routes for the given API.
  --
  -- Since @TypeApplications@ is becoming pervasive, we forego @Proxy@ here in favour
  -- of @getRoutes \@API@.
  getRoutes :: [Route]

-- | Get all the routes of an API and print them to stdout. See 'renderRoute' for examples.
printRoutes :: forall api. (HasRoutes api) => IO ()
printRoutes = traverse_ printRoute $ getRoutes @api
  where
    printRoute = T.putStrLn . renderRoute

{- | Get all the routes of an API, sort them by path and method, and print them to stdout.
 See 'renderRoute' for examples.
-}
printRoutesSorted :: forall api. (HasRoutes api) => IO ()
printRoutesSorted = traverse_ printRoute . sort $ getRoutes @api
  where
    printRoute = T.putStrLn . renderRoute

{- | Same as 'printRoutes`, but encode the t'Routes' as JSON before printing to stdout.
For an even prettier version, see 'printRoutesJSONPretty'.
-}
printRoutesJSON :: forall api. (HasRoutes api) => IO ()
printRoutesJSON =
  T.putStrLn
    . TL.toStrict
    . TLE.decodeUtf8
    . encode
    . Routes
    $ getRoutes @api

-- | Pretty-encode the t'Routes' as JSON before printing to stdout.
printRoutesJSONPretty :: forall api. (HasRoutes api) => IO ()
printRoutesJSONPretty =
  T.putStrLn
    . TL.toStrict
    . TLE.decodeUtf8
    . encodePretty
    . Routes
    $ getRoutes @api

instance HasRoutes EmptyAPI where
  getRoutes = mempty

instance
  (ReflectMethod (method :: StdMethod)) =>
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
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneResponse @a

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
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneResponse @(Headers hs a)

#if MIN_VERSION_servant(0,18,1)
instance
  {-# OVERLAPPING #-}
  (ReflectMethod (method :: StdMethod)) =>
  HasRoutes (UVerb method ctypes '[])
  where
  getRoutes = pure $ defRoute method
    where
      method = reflectMethod $ Proxy @method

instance
  {-# OVERLAPPING #-}
  (ReflectMethod (method :: StdMethod), Typeable a) =>
  HasRoutes (UVerb method ctypes '[a])
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneResponse @a

instance
  (ReflectMethod (method :: StdMethod), AllHasResponse as, Unique as) =>
  HasRoutes (UVerb method ctypes as)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneOfResponses @as
#endif

instance (HasRoutes l, HasRoutes r) => HasRoutes (l :<|> r) where
  getRoutes = getRoutes @l <> getRoutes @r

instance (KnownSymbol path, HasRoutes api) => HasRoutes (path :> api) where
  getRoutes = getRoutes @api <&> routePath %~ prependPathPart path
    where
      path = knownSymbolT @path

instance
  (KnownSymbol capture, Typeable a, HasRoutes api) =>
  HasRoutes (Capture' mods capture a :> api)
  where
  getRoutes = getRoutes @api <&> routePath %~ prependCapturePart @a capture
    where
      capture = knownSymbolT @capture

instance
  (KnownSymbol capture, Typeable a, HasRoutes api) =>
  HasRoutes (CaptureAll capture a :> api)
  where
  getRoutes = getRoutes @api <&> routePath %~ prependCaptureAllPart @a capture
    where
      capture = knownSymbolT @capture

instance
  (KnownSymbol sym, Typeable (RequiredArgument mods a), HasRoutes api) =>
  HasRoutes (QueryParam' mods sym a :> api)
  where
  getRoutes = getRoutes @api <&> routeParams `add` param
    where
      param = singleParam @sym @(RequiredArgument mods a)

instance
  (KnownSymbol sym, Typeable a, HasRoutes api) =>
  HasRoutes (QueryParams sym a :> api)
  where
  getRoutes = getRoutes @api <&> routeParams `add` param
    where
      param = arrayElemParam @sym @a

#if MIN_VERSION_servant(0,19,0)
instance (HasRoutes (ToServantApi routes)) => HasRoutes (NamedRoutes routes) where
  getRoutes = getRoutes @(ToServantApi routes)
#endif

instance (KnownSymbol sym, HasRoutes api) => HasRoutes (QueryFlag sym :> api) where
  getRoutes = getRoutes @api <&> routeParams `add` param
    where
      param = flagParam @sym

instance (HasRoutes api, Typeable a) => HasRoutes (ReqBody' mods list a :> api) where
  getRoutes = getRoutes @api <&> routeRequestBody <>~ reqBody
    where
      reqBody = oneRequest @a

instance (HasRoutes api) => HasRoutes (Vault :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (HttpVersion :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api, KnownSymbol realm) => HasRoutes (BasicAuth realm usr :> api) where
  getRoutes = getRoutes @api <&> routeAuths `add` auth
    where
      auth = basicAuth @realm

instance (HasRoutes api, KnownSymbol sym) => HasRoutes (Description sym :> api) where
  getRoutes = getRoutes @api <&> routeDescription %~ (<|> Just (ResponseDescription (knownSymbolT @sym)))

instance (HasRoutes api, KnownSymbol sym) => HasRoutes (Summary sym :> api) where
  getRoutes = getRoutes @api <&> routeSummary %~ (<|> Just (RouteSummary (knownSymbolT @sym)))

instance
  (HasRoutes api, KnownSymbol tag) =>
  HasRoutes (AuthProtect (tag :: Symbol) :> api)
  where
  getRoutes = getRoutes @api <&> routeAuths `add` auth
    where
      auth = customAuth @tag

instance
  (HasRoutes api, KnownSymbol sym, Typeable (RequiredArgument mods a)) =>
  HasRoutes (Header' mods sym a :> api)
  where
  getRoutes = getRoutes @api <&> routeRequestHeaders `add` header
    where
      header = mkHeaderRep @sym @(RequiredArgument mods a)

instance (HasRoutes api) => HasRoutes (Fragment v :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (IsSecure :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (RemoteHost :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api, Typeable a) => HasRoutes (StreamBody' mods framing ct a :> api) where
  getRoutes = getRoutes @api <&> routeRequestBody .~ reqBody
    where
      reqBody = oneRequest @a

instance (HasRoutes api) => HasRoutes (WithNamedContext name subContext api) where
  getRoutes = getRoutes @api

instance
  (ReflectMethod (method :: StdMethod), Typeable a) =>
  HasRoutes (Stream method status framing ctype a)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneResponse @a

#if MIN_VERSION_servant(0,20,3)
instance
  {-# OVERLAPPING #-}
  ( ReflectMethod method
  , AsUnion '[] result -- I don't think this will ever be satisfied, but you never know.
  ) =>
  HasRoutes (MultiVerb (method :: StdMethod) (mimeTypes :: [k]) '[] result)
  where
  getRoutes = pure $ defRoute method
    where
      method = reflectMethod $ Proxy @method

instance
  {-# OVERLAPPING #-}
  ( ReflectMethod method
  , HasResponse response
  , AsUnion '[response] result
  ) =>
  HasRoutes (MultiVerb (method :: StdMethod) (mimeTypes :: [k]) '[response :: Type] result)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneResponse @response

instance
  ( ReflectMethod method
  , AllHasResponse responses
  , AsUnion responses result
  ) =>
  HasRoutes (MultiVerb (method :: StdMethod) (mimeTypes :: [k]) (responses :: [Type]) result)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponse .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneOfResponses @responses
#endif

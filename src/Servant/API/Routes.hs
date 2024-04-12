{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Servant.API.Routes
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

This package provides two things:

  1. A simple, and probably incomplete, way to represent APIs at the term level.
     This is achieved by the 'Route', t'Routes', 'Path', 'Param', 'HeaderRep' types.
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

/Note that 'printRoutes' only includes the path, method and query parameters.
For more detailed comparison, use the JSON instance of t'Routes', encode the routes to
a file (before and after the refactoring), and use [jdiff](https://github.com/networktocode/jdiff)./

Another use-case is in testing: some Haskellers use type families to modify Servant APIs, for example
to add endpoints or authorisation headers. Types are hard to test, but terms are easy. Use 'HasRoutes'
and run your tests on t'Routes'.
-}
module Servant.API.Routes
  ( -- * API routes
    Routes
  , unRoutes
  , pattern Routes
  , Route
  , defRoute

    -- * Automatic generation of routes for Servant API types

    -- | Now we can automatically generate a t'Routes' for any Servant combinator. In most cases
    -- the user should never need to implement 'HasRoutes' unless they're hacking on Servant or
    -- defining their own combinators.
  , HasRoutes (..)
  , printRoutes
  )
where

import Data.Aeson
import qualified Data.Aeson.Key as AK (fromText)
import qualified Data.Aeson.Types as A (Pair)
import Data.Bifunctor (bimap)
import Data.Foldable (foldl', traverse_)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol)
import Lens.Micro
import Network.HTTP.Types.Method (Method)
import Servant.API
import Servant.API.Modifiers (RequiredArgument)
import "this" Servant.API.Routes.Body
import "this" Servant.API.Routes.Header
import "this" Servant.API.Routes.Internal.Body
import "this" Servant.API.Routes.Param
import "this" Servant.API.Routes.Path
import "this" Servant.API.Routes.Route
import "this" Servant.API.Routes.Utils

{- | To render all of an API's 'Route's as JSON, we need to identify each route by its
 path AND its method (since 2 routes can have the same path but different method).
 This newtype lets us represent this nested structure.
-}
newtype Routes = UnsafeRoutes
  { unRoutes :: Map.Map Path (Map.Map Method Route)
  -- ^ Get the underlying 'Map.Map' of a t'Routes'.
  }
  deriving (Show, Eq)

makeRoutes :: [Route] -> Routes
makeRoutes = UnsafeRoutes . foldl' insert mempty
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

However, if we user 'HasRoutes' as a sanity check:

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
>         "response": "[User]",
>         "response_headers": []
>     },
>     {
>         "auths": [],
>         "method": "POST",
>         "params": [],
>         "path": "/api/v2/user",
>         "request_body": "UserData",
>         "request_headers": [],
>         "response": "UserId",
>         "response_headers": []
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
>         "response": "[User]",
>         "response_headers": []
>     },
>     {
>         "auths": [],
>         "method": "POST",
>         "params": [],
>         "path": "/user",              -- oops!
>         "request_body": "UserData",
>         "request_headers": [],
>         "response": "UserId",
>         "response_headers": []
>     }
> ]
-}
class HasRoutes api where
  -- | Type-level list of API routes for the given API.
  --
  -- Since @TypeApplications@ is becoming pervasive, we forego @Proxy@ here in favour
  -- of @getRoutes \@API@.
  getRoutes :: [Route]

-- | Get all the routes of an API and print them to stdout. See 'showRoute' for examples.
printRoutes :: forall api. HasRoutes api => IO ()
printRoutes = traverse_ printRoute $ getRoutes @api
  where
    printRoute = T.putStrLn . showRoute

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
        & routeResponseType .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneType @a

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
        & routeResponseType .~ response
    where
      method = reflectMethod $ Proxy @method
      headers = getHeaderReps @hs
      response = oneType @a

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
        & routeResponseType .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneType @a

instance
  (ReflectMethod (method :: StdMethod), AllTypeable as, Unique as) =>
  HasRoutes (UVerb method ctypes as)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponseType .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneOf @as

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

#if MIN_VERSION_servant(0,19,0)
instance (HasRoutes (ToServantApi routes)) => HasRoutes (NamedRoutes routes) where
  getRoutes = getRoutes @(ToServantApi routes)
#endif

instance (KnownSymbol sym, HasRoutes api) => HasRoutes (QueryFlag sym :> api) where
  getRoutes = getRoutes @api <&> routeParams %~ (param :)
    where
      param = flagParam @sym

instance (HasRoutes api, Typeable a) => HasRoutes (ReqBody' mods list a :> api) where
  getRoutes = getRoutes @api <&> routeRequestBody <>~ reqBody
    where
      reqBody = oneType @a

instance (HasRoutes api) => HasRoutes (Vault :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (HttpVersion :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api, KnownSymbol realm) => HasRoutes (BasicAuth realm usr :> api) where
  getRoutes = getRoutes @api <&> routeAuths %~ (auth :)
    where
      auth = "Basic " <> knownSymbolT @realm

instance (HasRoutes api) => HasRoutes (Description sym :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (Summary sym :> api) where
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

instance (HasRoutes api) => HasRoutes (Fragment v :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (IsSecure :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api) => HasRoutes (RemoteHost :> api) where
  getRoutes = getRoutes @api

instance (HasRoutes api, Typeable a) => HasRoutes (StreamBody' mods framing ct a :> api) where
  getRoutes = getRoutes @api <&> routeRequestBody .~ reqBody
    where
      reqBody = oneType @a

instance (HasRoutes api) => HasRoutes (WithNamedContext name subContext api) where
  getRoutes = getRoutes @api

instance
  (ReflectMethod (method :: StdMethod), Typeable a) =>
  HasRoutes (Stream method status framing ctype a)
  where
  getRoutes =
    pure $
      defRoute method
        & routeResponseType .~ response
    where
      method = reflectMethod $ Proxy @method
      response = oneType @a

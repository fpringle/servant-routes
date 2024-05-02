{- |
Module      : Servant.API.Routes.Request
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Term-level representation of the request bodies that Servant endpoints expect.
-}
module Servant.API.Routes.Request
  ( Request
  , unRequest
  , noRequest
  , oneRequest
  , allOfRequests
  , requests
  )
where

import Data.Typeable
import Lens.Micro
import "this" Servant.API.Routes.Internal.Request
import "this" Servant.API.Routes.Internal.Some
import "this" Servant.API.Routes.Utils

-- | The endpoint doesn't expect a request body.
noRequest :: Request
noRequest = Request None

{- | The request body can only be of one type. Equivalent to a single
@'Servant.API.ReqBody' _ a@.
-}
oneRequest ::
  forall a.
  Typeable a =>
  Request
oneRequest = Request . One $ typeRepOf @a

{- | The endpoint expects the request body to be parsed as multiple (>1) types.
Equivalent to multiple 'Servant.API.ReqBody's chained with @:>@.
-}
allOfRequests ::
  forall as.
  AllTypeable as =>
  Request
allOfRequests = Request . Many $ typeReps @as

-- | Convenience optic to traverse over all the 'TypeRep's within a 'Request'.
requests :: Traversal' Request TypeRep
requests = unRequest . traversed

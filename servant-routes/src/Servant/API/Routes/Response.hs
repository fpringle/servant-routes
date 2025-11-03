{- |
Module      : Servant.API.Routes.Response
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Term-level representation of the responses that Servant endpoints can return.
-}
module Servant.API.Routes.Response
  ( Responses
  , unResponses
  , noResponse
  , oneResponse
  , oneOfResponses
  , Response
  , responseType
  , responseHeaders
  , responseDescription
  , responses
  , HasResponse (..)
  , AllHasResponse (..)
  )
where

import Lens.Micro
import "this" Servant.API.Routes.Internal.Response
import "this" Servant.API.Routes.Internal.Some

-- | The endpoint will not return a response.
noResponse :: Responses
noResponse = Responses None

-- | There is only one possible response. Equivalent to a single @'Servant.API.ReqBody' _ a@.
oneResponse ::
  forall a.
  (HasResponse a) =>
  Responses
oneResponse = Responses . One $ getResponse @a

{- | The endpoint may return one of multiple multiple (>1) responses.
Equivalent to a 'Servant.API.UVerb's with more than one type.
-}
oneOfResponses ::
  forall as.
  (AllHasResponse as) =>
  Responses
oneOfResponses = Responses . Many $ getResponses @as

-- | Convenience optic to traverse over all the 'Response's within a 'Responses'.
responses :: Traversal' Responses Response
responses = unResponses . traversed

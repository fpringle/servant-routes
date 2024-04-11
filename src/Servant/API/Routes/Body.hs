{- |
Module      : Servant.API.Routes.Body
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

The 'Body' type lets us represent HTTP bodies that may have interpretations as several different types.
-}
module Servant.API.Routes.Body
  ( Body
  , noBody
  , oneType
  , manyTypes
  , allOf
  , oneOf
  )
where

import Data.Typeable
import "this" Servant.API.Routes.Internal.Body
import "this" Servant.API.Routes.Utils

-- | There is no request/response body. Equialent to 'Nothing' or 'Data.Aeson.Null'.
noBody :: Body
noBody = NoBody

{- | The request/response body can only be of one type. Equivalent to a single
@'Servant.API.ReqBody' _ a@.
-}
oneType ::
  forall a.
  Typeable a =>
  Body
oneType = OneType $ typeRepOf @a

-- | The body admits multiple representations. For more expressive aliases, use 'allOf' or 'oneOf'.
manyTypes ::
  forall as.
  AllTypeable as =>
  Body
manyTypes = ManyTypes $ typeReps @as

{- | The request body must satisfy several 'Data.Aeson.FromJSON' instances. Equivalent to multiple
'Servant.API.ReqBody''s chained with @:>@, or OpenApi's
[allOf](https://swagger.io/docs/specification/data-models/oneof-anyof-allof-not/#allof).

Alias for 'manyTypes'.
-}
allOf ::
  forall as.
  AllTypeable as =>
  Body
allOf = manyTypes @as

{- | The response body could be one of several types. Equivalent to the 'Servant.API.UVerb'
combinator, or OpenApi's [oneOf](https://swagger.io/docs/specification/data-models/oneof-anyof-allof-not/#oneof).

Alias for 'manyTypes'.
-}
oneOf ::
  forall as.
  AllTypeable as =>
  Body
oneOf = manyTypes @as

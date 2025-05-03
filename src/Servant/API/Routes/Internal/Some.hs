{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Some
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.

For both requests and responses, we want to represent three situations, depending on the number
of /X/ present: 0, 1, or many.

For example, if multiple 'Servant.API.ReqBody''s are chained together with @:>@, the resulting
type's @HasServer@ instance would try to parse the request body as all of the relevant types. In this
case the 'Servant.API.Routes.Internal.Route._routeRequestBody' field needs to be able to contain several
'Data.Typeable.TypeRep's as a conjunction (AND).

On the other hand, the 'Servant.API.UVerb' combinator lets us represent an endpoint that may return one of
several types: hence the 'Servant.API.Routes.Internal.Route._routeResponse' field needs to be able to contain
several possible responses as a disjunction (OR).

The 'Some' type lets us represent both of these situations.

However we need to abstract over the type contained in 'Some', since we need to represent different
data in those 2 different situations, because of the way that Servant represents them:

  - for requests, different 'Servant.API.ReqBody's and 'Servant.API.Header.Header's are independent.
    Therefore in 'Servant.API.Routes.Route.Route', for requests we have
    @'Servant.API.Routes.Internal.Route._routeRequestHeaders' :: 'Servant.API.Routes.Header.HeaderRep'@ and
    @'Servant.API.Routes.Internal.Route._routeRequestBody' ~ 'Some' 'Data.Typeable.TypeRep'@.
  - for responses, multiple response options are represented using 'Servant.API.UVerb', which bundles response
    types and headers together. Therefore for responses we have just one field,
    @'Servant.API.Routes.Internal.Route._routeResponse' ~ 'Some' ('Data.Typeable.TypeRep', ['Servant.API.Routes.Header.HeaderRep'])@.
-}
module Servant.API.Routes.Internal.Some
  ( Some (..)
  , toList
  , fromList
  , eqSome
  , appendSome
  , someToJSONAs
  )
where

import Data.Aeson
import qualified Data.Aeson.Key as AK (fromText)
import qualified Data.Foldable as Fold
import qualified Data.Text as T

-- | Simple ADT which codifies whether a list contains 0, 1, or many elements.
data Some a
  = None
  | One a
  | -- | Invariant: list needs to have length > 1
    --
    -- Whether or not order is important is left up to the user. Therefore we define no instances of
    -- 'Eq', 'Ord', 'Semigroup' or any other class that would involve comparing or combining this list.
    Many [a]
  deriving (Show, Functor, Foldable, Traversable)

{- | Compare 2 'Some's for equality, given a way to compare lists in the 'Many' constructor.

Use this to implement `Eq` instances for newtypes around 'Some'.
-}
eqSome :: forall a. Eq a => ([a] -> [a] -> Bool) -> Some a -> Some a -> Bool
eqSome eqList = eq
  where
    None `eq` None = True
    One t1 `eq` One t2 = t1 == t2
    Many ts1 `eq` Many ts2 = ts1 `eqList` ts2
    _ `eq` _ = False

{- | Combine 2 'Some's, given a way to combine single elements with lists in the
@'Servant.API.Routes.Internal.Some.One' <> 'Many'@ cases.

Use this to implement `Semigroup` instances for newtypes around 'Some'.
-}
appendSome ::
  forall a.
  (a -> [a] -> [a]) ->
  ([a] -> a -> [a]) ->
  Some a ->
  Some a ->
  Some a
appendSome cons' snoc' = app
  where
    None `app` x = x
    x `app` None = x
    One t1 `app` One t2 = Many [t1, t2]
    One t `app` Many ts = Many (t `cons'` ts)
    Many ts `app` One t = Many (ts `snoc'` t)
    Many ts1 `app` Many ts2 = Many (ts1 <> ts2)

-- | Convert a 'Some' to a list. Inverse of 'fromList'.
toList :: Some a -> [a]
toList = Fold.toList

{- | Convert a list of @a@s to a 'Some'. Inverse of 'toList'.

This maintains the invariant that the argument of 'Many' has to be of length > 1.
-}
fromList :: [a] -> Some a
fromList = \case
  [] -> None
  [tRep] -> One tRep
  tReps -> Many tReps

{- | Represent a 'Some' as a JSON 'Value'.

Use this to implement `ToJSON` instances for newtypes around 'Some'.

This choice of representation is opinionated, and some may disagree.

Given a function @f@ to convert an @a@ to JSON, and a @label@:

@
'None' -> null
'Servant.API.Routes.Internal.Some.One' a -> f a
'Many' list -> { label: map f list }
@
-}
someToJSONAs :: (a -> Value) -> T.Text -> Some a -> Value
someToJSONAs aToJSON lbl = \case
  None -> Null
  One tRep -> aToJSON tRep
  Many tReps ->
    object [AK.fromText lbl .= fmap aToJSON tReps]

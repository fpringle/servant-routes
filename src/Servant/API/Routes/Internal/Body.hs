{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Body
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Body
  ( Body (..)
  , bodyToList
  , listToBody
  , AllTypeable (..)
  )
where

import Data.Kind (Type)
import Data.List (nub, sort)
import Data.Typeable
import "this" Servant.API.Routes.Utils

{- | A more expressive sum-type than just a list. This can be useful in situations
where a body (request or response) may have several interpretations.

For example, the 'Servant.API.UVerb' combinator lets us represent an endpoint that may return one of
several types: hence the 'Servant.API.Routes._routeResponseType' field needs to be able to contain
several 'TypeRep's as a disjunction.

On the other hand, if multiple 'Servant.API.ReqBody''s are chained together with @:>@, the resulting
type's @HasServer@ instance would try to parse the request body as all of the relevant types. In this
case the 'Servant.API.Routes._routeRequestBody' field needs to be able to contain several 'TypeRep's
as a conjunction.
-}
data Body
  = NoBody
  | OneType TypeRep
  | -- | invariant: list needs to have length > 1
    ManyTypes [TypeRep] -- order not important
  deriving (Show)

-- | Convert a 'Body' to a list of 'TypeRep's. Inverse of 'listToBody'.
bodyToList :: Body -> [TypeRep]
bodyToList = \case
  NoBody -> []
  OneType tRep -> [tRep]
  ManyTypes tReps -> tReps

{- | Convert a list of 'TypeRep's to a 'Body'. Inverse of 'listToBody'.

This maintains the invariant that the argument of 'ManyTypes' has to be of length > 1.
-}
listToBody :: [TypeRep] -> Body
listToBody = \case
  [] -> NoBody
  [tRep] -> OneType tRep
  tReps -> ManyTypes tReps

instance Eq Body where
  NoBody == NoBody = True
  OneType t1 == OneType t2 = t1 == t2
  ManyTypes ts1 == ManyTypes ts2 = sort (nub ts1) == sort (nub ts2)
  _ == _ = False

instance Semigroup Body where
  NoBody <> x = x
  x <> NoBody = x
  OneType t1 <> OneType t2 = ManyTypes [t1, t2]
  OneType t <> ManyTypes ts = ManyTypes (t : ts)
  ManyTypes ts <> OneType t = ManyTypes (t : ts) -- order not important, more efficient
  ManyTypes ts1 <> ManyTypes ts2 = ManyTypes (ts1 <> ts2)

instance Monoid Body where
  mempty = NoBody

{- | This class does 2 things:

- It lets us get a term-level list of 'TypeRep's from a type-level list of types, all of
  which have 'Typeable' instances.
- More impressively, its instances enforce that 'typeReps' will only type-check for type-level
  lists of length 2 or more. This is because 'AllTypeable' will only ever be used by
  'Servant.API.Routes.Body.manyTypes' (and its aliases), which is the only way to construct a
  'ManyTypes' and thus lets us enforce the invariant that 'ManyTypes' will always have more
  than 1 argument. This lets us make sure that there's only ever one way to represent a list of
  'TypeRep's using 'Body'.

  Of course, someone might import this Internal module and define a @'Typeable' a => 'AllTypeable' '[a]@
  instance. Don't do that.
-}
class AllTypeable (as :: [Type]) where
  typeReps :: [TypeRep]

instance (Typeable a, Typeable b) => AllTypeable '[a, b] where
  typeReps = [typeRepOf @a, typeRepOf @b]

instance (Typeable a, AllTypeable (b ': c ': as)) => AllTypeable (a ': b ': c ': as) where
  typeReps = typeRepOf @a : typeReps @(b ': c ': as)

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Request
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Request
  ( Request (..)
  , unRequest
  , AllTypeable (..)
  )
where

import Data.Aeson
import Data.Function (on)
import Data.Kind (Type)
import Data.List (nub, sort)
import Data.Typeable
import Lens.Micro.TH
import "this" Servant.API.Routes.Internal.Some as S
import "this" Servant.API.Routes.Utils

{- | A representation of the request /body/(s) that a Servant endpoint expects.

Under the hood, 'Request' is a @'Some' 'TypeRep'@.
This allows for the possibility that an endpoint might expect the request body
to parse as several different types (multiple 'Servant.API.ReqBody''s).

Note that this type doesn't include any information about the headers that an
endpoint expects, since those are independent of the request body.
-}
newtype Request = Request {_unRequest :: Some TypeRep}
  deriving (Show) via Some TypeRep

makeLenses ''Request

instance ToJSON Request where
  toJSON = someToJSONAs typeRepToJSON "all_of" . _unRequest

instance Eq Request where
  (==) = eqSome ((==) `on` (sort . nub)) `on` _unRequest

instance Semigroup Request where
  Request b1 <> Request b2 = Request (appendSome (:) (flip (:)) b1 b2)

instance Monoid Request where
  mempty = Request S.None

{- | This class does 2 things:

- It lets us get a term-level list of 'TypeRep's from a type-level list of types, all of
  which have 'Typeable' instances.
- More impressively, its instances enforce that 'typeReps' will only type-check for type-level
  lists of length 2 or more. This is because 'AllTypeable' will only ever be used by
  'Servant.API.Routes.Request.allOfRequests', which is the only way to construct a
  'Many' @'Request' and thus lets us enforce the invariant that its list arguments will always
  have more than 1 element. This lets us make sure that there's only ever one way to represent a list of
  'TypeRep's using 'Request'.

  Of course, someone might import this Internal module and define a @'Typeable' a => 'AllTypeable' '[a]@
  instance. Don't do that.
-}
class AllTypeable (as :: [Type]) where
  typeReps :: [TypeRep]

instance (Typeable a, Typeable b) => AllTypeable '[a, b] where
  typeReps = [typeRepOf @a, typeRepOf @b]

instance (Typeable a, AllTypeable (b ': c ': as)) => AllTypeable (a ': b ': c ': as) where
  typeReps = typeRepOf @a : typeReps @(b ': c ': as)

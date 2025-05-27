{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Response
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Response
  ( Responses (..)
  , unResponses
  , Response (..)
  , responseType
  , responseHeaders
  , HasResponse (..)
  , AllHasResponse (..)
  )
where

import Data.Aeson
import Data.Function (on)
import Data.Kind (Type)
import Data.List (nub, sort)
import qualified Data.Set as Set
import Data.Typeable
import Lens.Micro
import Lens.Micro.TH
import Servant.API hiding (getResponse)
import "this" Servant.API.Routes.Internal.Header
import "this" Servant.API.Routes.Internal.Some as S
import "this" Servant.API.Routes.Utils

{- | A representation of /one/ possible response that a Servant endpoint
can return.

Currently, the only situation in which multiple 'Response's can be returned
is using the 'UVerb' combinator. This bundles response /types/ together with
response 'Servant.API.Header.Header's, so we do the same here.
-}
data Response = Response
  { _responseType :: TypeRep
  , _responseHeaders :: Set.Set HeaderRep
  }
  deriving (Show, Eq, Ord)

makeLenses ''Response

instance ToJSON Response where
  toJSON Response {..} =
    object
      [ "type" .= typeRepToJSON _responseType
      , "headers" .= _responseHeaders
      ]

{- | Get a term-level response from a type-level argument. This encodes the argument(s)
of a 'Verb' or 'UVerb'.

Similar to 'Typeable', but also get the response 'Servant.API.Header.Header's.
-}
class HasResponse a where
  getResponse :: Response

instance {-# OVERLAPPABLE #-} (Typeable a) => HasResponse a where
  getResponse = Response (typeRepOf @a) mempty

instance {-# OVERLAPPING #-} (HasResponse a, GetHeaderReps hs) => HasResponse (Headers hs a) where
  getResponse =
    getResponse @a
      & responseHeaders <>~ Set.fromList (getHeaderReps @hs)

{- | Witness that all members of a type-level list are instances of 'HasResponse'.

This class does 2 things:

- It lets us get a term-level list of 'Response's from a type-level list of types, all of
  which have 'HasResponse' instances.
- More impressively, its instances enforce that 'getResponses' will only type-check for type-level
  lists of length 2 or more. This is because 'AllHasResponse' will only ever be used by
  'Servant.API.Routes.Response.oneOfResponses', which is the only way to construct a
  'Many' @'Response' and thus lets us enforce the invariant that its list arguments will always
  have more than 1 element. This lets us make sure that there's only ever one way to represent a list of
  'Response's using 'Responses'.

  Of course, someone might import this Internal module and define a @'HasResponse' a => 'AllHasResponse' '[a]@
  instance. Don't do that.
-}
class AllHasResponse (as :: [Type]) where
  getResponses :: [Response]

instance (HasResponse a, HasResponse b) => AllHasResponse '[a, b] where
  getResponses = [getResponse @a, getResponse @b]

instance (HasResponse a, AllHasResponse (b ': c ': as)) => AllHasResponse (a ': b ': c ': as) where
  getResponses = getResponse @a : getResponses @(b ': c ': as)

{- | A representation of the response(s) that a Servant endpoint can return.

Under the hood, 'Responses' is a @'Some' 'Response'@.
This allows for the possibility that an endpoint might return one of several
responses, via 'UVerb'.

Note that a 'Response' consists of a return body type, /as well as/ the return headers.
-}
newtype Responses = Responses {_unResponses :: Some Response}
  deriving (Show) via Some Response

makeLenses ''Responses

instance Eq Responses where
  (==) = eqSome ((==) `on` (sort . nub)) `on` _unResponses

instance Semigroup Responses where
  Responses b1 <> Responses b2 = Responses (appendSome (:) (flip (:)) b1 b2)

instance Monoid Responses where
  mempty = Responses S.None

instance ToJSON Responses where
  toJSON = someToJSONAs toJSON "one_of" . _unResponses

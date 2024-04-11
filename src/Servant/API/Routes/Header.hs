{- |
Module      : Servant.API.Routes.Header
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Simple representation of HTTP headers.
-}
module Servant.API.Routes.Header
  ( HeaderRep
  , GetHeaderReps (..)
  , mkHeaderRep
  )
where

import Data.Kind (Type)
import Data.Typeable
import GHC.TypeLits
import Servant.API
import "this" Servant.API.Routes.Internal.Header
import "this" Servant.API.Routes.Utils

-- | Convenience function to construct a 'HeaderRep' from @sym :: 'Symbol'@ and @a :: Type'@.
mkHeaderRep ::
  forall sym a.
  (KnownSymbol sym, Typeable a) =>
  HeaderRep
mkHeaderRep =
  HeaderRep
    { _hName = knownSymbolT @sym
    , _hType = typeRepOf @a
    }

{- | Utility class to let us get a value-level list of 'HeaderRep's from a
type-level list of 'Servant.API.Header.Header's. See the implementation of
@'Servant.API.Route.HasRoutes' ('Verb' method status ctypes ('Headers' hs a))@ for an example.
-}
class GetHeaderReps (hs :: [Type]) where
  getHeaderReps :: [HeaderRep]

instance GetHeaderReps '[] where
  getHeaderReps = []

instance
  (GetHeaderReps rest, KnownSymbol h, Typeable v) =>
  GetHeaderReps (Header h v ': rest)
  where
  getHeaderReps = header : getHeaderReps @rest
    where
      header = mkHeaderRep @h @v

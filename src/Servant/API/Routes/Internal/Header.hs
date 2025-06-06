{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Header
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Header
  ( HeaderRep (..)
  , mkHeaderRep
  , GetHeaderReps (..)
  )
where

import Data.Aeson
import Data.Kind (Type)
import Data.Text
import Data.Typeable
import GHC.TypeLits
import Servant.API
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

{- | Simple term-level representation of a 'Servant.API.Header.Header'.

A type-level @'Servant.API.Header.Header' (sym :: 'GHC.TypeLits.Symbol') typ@ should correspond to
@'HeaderRep' { _hName = str, _hType =  typRep }@, where @str@ is the term-level equivalent
of @sym@ and @typRep@ is the term-level representation of @typ@.
-}
data HeaderRep = HeaderRep
  { _hName :: Text
  , _hType :: TypeRep
  }
  deriving (Show, Eq, Ord)

instance ToJSON HeaderRep where
  toJSON HeaderRep {..} =
    object
      [ "name" .= _hName
      , "type" .= typeRepToJSON _hType
      ]

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

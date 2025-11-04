{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , GetHeaderRep (..)
  )
where

import Data.Aeson
import Data.Kind (Type)
import Data.Text
import GHC.TypeLits
import Servant.API
import Type.Reflection
import "this" Servant.API.Routes.Utils

-- | Convenience function to construct a 'HeaderRep' from @sym :: 'Symbol'@ and @a :: Type'@.
mkHeaderRep ::
  forall sym a.
  (KnownSymbol sym, Typeable a) =>
  HeaderRep
mkHeaderRep =
  HeaderRep
    { _hName = knownSymbolT @sym
    , _hType = typeRep @a
    }

{- | Simple term-level representation of a 'Servant.API.Header.Header'.

A type-level @'Servant.API.Header.Header' (sym :: 'GHC.TypeLits.Symbol') typ@ should correspond to
@'HeaderRep' { _hName = str, _hType =  typRep }@, where @str@ is the term-level equivalent
of @sym@ and @typRep@ is the term-level representation of @typ@.
-}
data HeaderRep where
  HeaderRep ::
    forall (a :: Type).
    { _hName :: Text
    , _hType :: TypeRep a
    } ->
    HeaderRep

deriving instance Show HeaderRep

instance Eq HeaderRep where
  HeaderRep n1 t1 == HeaderRep n2 t2 =
    n1 == n2 && (SomeTypeRep t1 == SomeTypeRep t2)

instance Ord HeaderRep where
  HeaderRep n1 t1 `compare` HeaderRep n2 t2 =
    n1 `compare` n2 <> (SomeTypeRep t1 `compare` SomeTypeRep t2)

instance ToJSON HeaderRep where
  toJSON HeaderRep {..} =
    object
      [ "name" .= _hName
      , "type" .= typeRepToJSON (SomeTypeRep _hType)
      ]

class GetHeaderRep h where
  getHeaderRep :: HeaderRep

instance
  (KnownSymbol h, Typeable v) =>
  GetHeaderRep (Header h v)
  where
  getHeaderRep = mkHeaderRep @h @v

{- | Utility class to let us get a value-level list of 'HeaderRep's from a
type-level list of 'Servant.API.Header.Header's. See the implementation of
@'Servant.API.Route.HasRoutes' ('Verb' method status ctypes ('Headers' hs a))@ for an example.
-}
class GetHeaderReps (hs :: [Type]) where
  getHeaderReps :: [HeaderRep]

instance GetHeaderReps '[] where
  getHeaderReps = []

instance
  (GetHeaderReps rest, GetHeaderRep h) =>
  GetHeaderReps (h ': rest)
  where
  getHeaderReps = getHeaderRep @h : getHeaderReps @rest

{-# LANGUAGE CPP #-}
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
  , mkHeaderRepOptional
  , GetHeaderReps (..)
  , GetHeaderRep (..)
  )
where

import Data.Aeson
import Data.Kind (Type)
import Data.Text
import GHC.TypeLits
import Servant.API
#if MIN_VERSION_servant(0,20,3)
import Servant.API.MultiVerb
#endif
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

{- | Make the 'TypeRep' in the '_hType' field optional (using 'Maybe'). If the field is already
optional, leave it as is.

For example:

@
ghci> mkHeaderRep @"sym" @Int
HeaderRep {_hName = "sym", _hType = Int}

ghci> mkHeaderRepOptional $ mkHeaderRep @"sym" @Int
HeaderRep {_hName = "sym", _hType = Maybe Int}

ghci> mkHeaderRepOptional . mkHeaderRepOptional $ mkHeaderRep @"sym" @Int
HeaderRep {_hName = "sym", _hType = Maybe Int}
@
-}
mkHeaderRepOptional :: HeaderRep -> HeaderRep
mkHeaderRepOptional h@(HeaderRep _ (App (Con tc) _))
  | tc == typeRepTyCon (typeRep @Maybe) = h
mkHeaderRepOptional (HeaderRep name (r :: TypeRep a)) =
  HeaderRep name $ App (typeRep @Maybe) r

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

#if MIN_VERSION_servant(0,20,3)
instance (KnownSymbol name, Typeable v) =>
  GetHeaderRep (DescHeader name desc v)
  where
  getHeaderRep = mkHeaderRep @name @v

instance (GetHeaderRep h) =>
  GetHeaderRep (OptHeader h)
  where
  getHeaderRep =
    let hRep = getHeaderRep @h
    in  mkHeaderRepOptional hRep
#endif

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

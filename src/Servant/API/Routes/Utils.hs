{- |
Module      : Servant.API.Routes.Utils
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Common useful functions.
-}
module Servant.API.Routes.Utils
  ( knownSymbolT
  , typeRepToJSON
  , showTypeRep
  , typeRepOf
  )
where

import Data.Aeson
import Data.Kind
import qualified Data.Text as T
import Data.Typeable
import GHC.TypeLits

-- | Get the term-level equivalent of a 'Symbol' as a 'T.Text'.
knownSymbolT :: forall name. KnownSymbol name => T.Text
knownSymbolT = T.pack . symbolVal $ Proxy @name

-- | Convert a 'TypeRep' to a JSON 'Value' via its 'Show' instance.
typeRepToJSON :: TypeRep -> Value
typeRepToJSON = toJSON . show @TypeRep

-- | Get the 'TypeRep' of a 'Typeable' type without having to mess around with 'Proxy'.
typeRepOf :: forall a. Typeable a => TypeRep
typeRepOf = typeRep $ Proxy @a

-- | Get the 'TypeRep' of a 'Typeable' type as a 'T.Text'.
showTypeRep :: forall (a :: Type). Typeable a => T.Text
showTypeRep = T.pack . show $ typeRepOf @a

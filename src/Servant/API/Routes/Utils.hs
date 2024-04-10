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
  )
where

import Data.Aeson
import qualified Data.Text as T
import Data.Typeable
import GHC.TypeLits

knownSymbolT :: forall name. KnownSymbol name => T.Text
knownSymbolT = T.pack . symbolVal $ Proxy @name

typeRepToJSON :: TypeRep -> Value
typeRepToJSON = toJSON . show @TypeRep

showTypeRep :: forall a. Typeable a => T.Text
showTypeRep = T.pack . show . typeRep $ Proxy @a

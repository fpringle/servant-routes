module Servant.API.Routes.Util where

import Data.Typeable

intTypeRep :: TypeRep
intTypeRep = typeRep $ Proxy @Int

strTypeRep :: TypeRep
strTypeRep = typeRep $ Proxy @String

unitTypeRep :: TypeRep
unitTypeRep = typeRep $ Proxy @()

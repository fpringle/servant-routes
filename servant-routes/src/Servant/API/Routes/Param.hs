{- |
Module      : Servant.API.Routes.Param
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Simple representation of HTTP query params
-}
module Servant.API.Routes.Param
  ( Param
  , singleParam
  , arrayElemParam
  , flagParam
  , renderParam
  )
where

import qualified Data.Text as T
import Data.Typeable
import GHC.TypeLits
import qualified Servant.Links as S
import "this" Servant.API.Routes.Internal.Param
import "this" Servant.API.Routes.Utils

-- | Create a 'S.SingleParam' from a 'Symbol' and a 'TypeRep' via 'Typeable'.
singleParam :: forall s a. (KnownSymbol s, Typeable a) => Param
singleParam = Param (S.SingleParam name rep)
  where
    rep = showTypeRep @a
    name = symbolVal $ Proxy @s

-- | Create an 'S.ArrayParam' from a 'Symbol' and a 'TypeRep' via 'Typeable'.
arrayElemParam :: forall s a. (KnownSymbol s, Typeable a) => Param
arrayElemParam = Param (S.ArrayElemParam name rep)
  where
    rep = showTypeRep @a
    name = symbolVal $ Proxy @s

-- | Create a 'S.FlagParam' from a 'Symbol'.
flagParam :: forall s. (KnownSymbol s) => Param
flagParam = Param (S.FlagParam name)
  where
    name = symbolVal $ Proxy @s

-- | Pretty-print a 'Param'. Used by 'Servant.API.Routes.renderRoute'.
renderParam :: Param -> T.Text
renderParam (Param param) = case param of
  S.SingleParam var typ -> T.pack var <> "=<" <> typ <> ">"
  S.ArrayElemParam var typ -> T.pack var <> "=<[" <> typ <> "]>"
  S.FlagParam var -> T.pack var

{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Servant.API.Routes.Param
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

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

import Data.Aeson
import Data.Function (on)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import "this" Servant.API.Routes.Utils
import qualified Servant.Links as S

{- | Newtype wrapper around servant's 'S.Param' so we can define a sensible
'Eq' instance for it.
-}
newtype Param = Param
  { unParam :: S.Param
  }
  deriving (Show) via S.Param

instance Eq Param where
  (==) = eq `on` unParam
    where
      S.SingleParam name1 rep1 `eq` S.SingleParam name2 rep2 =
        name1 == name2 && rep1 == rep2
      S.ArrayElemParam name1 rep1 `eq` S.ArrayElemParam name2 rep2 =
        name1 == name2 && rep1 == rep2
      S.FlagParam name1 `eq` S.FlagParam name2 =
        name1 == name2
      _ `eq` _ = False

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

data ParamType
  = SingleParam
  | ArrayElemParam
  | FlagParam
  deriving (Show, Eq, Enum, Bounded, Generic)
  deriving (ToJSON)

instance ToJSON Param where
  toJSON (Param p) =
    object $ case p of
      S.SingleParam name rep ->
        withType SingleParam ["name" .= name, "param_type" .= rep]
      S.ArrayElemParam name rep ->
        withType ArrayElemParam ["name" .= name, "param_type" .= rep]
      S.FlagParam name ->
        withType FlagParam ["name" .= name]
    where
      withType t ps = ("type" .= t) : ps

-- | Pretty-print a 'Param'. Used by 'Servant.API.Routes.showRoute'.
renderParam :: Param -> T.Text
renderParam (Param param) = case param of
  S.SingleParam var typ -> T.pack var <> "=<" <> typ <> ">"
  S.ArrayElemParam var typ -> T.pack var <> "=<[" <> typ <> "]>"
  S.FlagParam var -> T.pack var

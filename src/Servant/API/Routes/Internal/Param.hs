{-# LANGUAGE DeriveAnyClass #-}

{- |
Module      : Servant.API.Routes.Internal.Param
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Param
  ( Param (..)
  )
where

import Data.Aeson
import Data.Function (on)
import GHC.Generics
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

instance Ord Param where
  compare = comp `on` unParam
    where
      S.SingleParam name1 rep1 `comp` S.SingleParam name2 rep2 =
        name1 `compare` name2 <> rep1 `compare` rep2
      S.ArrayElemParam name1 rep1 `comp` S.ArrayElemParam name2 rep2 =
        name1 `compare` name2 <> rep1 `compare` rep2
      S.FlagParam name1 `comp` S.FlagParam name2 =
        name1 `compare` name2
      S.SingleParam {} `comp` _ = LT
      _ `comp` S.SingleParam {} = LT
      S.ArrayElemParam {} `comp` _ = LT
      _ `comp` S.ArrayElemParam {} = LT

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

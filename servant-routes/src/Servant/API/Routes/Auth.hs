{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Auth
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Here we define a very very basic type to represent authentication schemes.
-}
module Servant.API.Routes.Auth
  ( Auth
  , basicAuth
  , customAuth
  )
where

import GHC.TypeLits (KnownSymbol)
import "this" Servant.API.Routes.Internal.Auth
import "this" Servant.API.Routes.Utils

{- | Create a term-level representation of a \"Basic" authentication scheme.

For example:

> ghci> toJSON $ basicAuth @"user"
> String "Basic user"
-}
basicAuth ::
  forall realm.
  (KnownSymbol realm) =>
  Auth
basicAuth = Basic $ knownSymbolT @realm

{- | Create a term-level representation of a \"Custom" authentication scheme, i.e. one that
corresponds to Servant's 'Servant.API.AuthProtect' combinator.

For example:

> ghci> toJSON $ customAuth @"OnlyAdminUsers"
> String "OnlyAdminUsers"
-}
customAuth ::
  forall tag.
  (KnownSymbol tag) =>
  Auth
customAuth = Custom $ knownSymbolT @tag

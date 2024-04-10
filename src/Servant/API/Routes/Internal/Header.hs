{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Header
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Header
  ( HeaderRep (..)
  )
where

import Data.Aeson
import Data.Text
import Data.Typeable
import "this" Servant.API.Routes.Utils

{- | Simple term-level representation of a 'Servant.API.Header.Header'.

A type-level @'Servant.API.Header.Header' (sym :: 'Symbol') typ@ should correspond to
@'HeaderRep' { _hName = str, _hType =  typRep }@, where @str@ is the term-level equivalent
of @sym@ and @typRep@ is the term-level representation of @typ@.
-}
data HeaderRep = HeaderRep
  { _hName :: Text
  , _hType :: TypeRep
  }
  deriving (Show, Eq)

instance ToJSON HeaderRep where
  toJSON HeaderRep {..} =
    object
      [ "name" .= _hName
      , "type" .= typeRepToJSON _hType
      ]

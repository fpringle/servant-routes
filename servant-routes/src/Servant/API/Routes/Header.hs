{- |
Module      : Servant.API.Routes.Header
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Simple representation of HTTP headers.
-}
module Servant.API.Routes.Header
  ( HeaderRep
  , GetHeaderReps (..)
  , GetHeaderRep (..)
  , mkHeaderRep
  )
where

import "this" Servant.API.Routes.Internal.Header

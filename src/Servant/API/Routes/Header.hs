{- |
Module      : Servant.API.Routes.Header
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Simple representation of HTTP headers.
-}
module Servant.API.Routes.Header
  ( HeaderRep
  , GetHeaderReps (..)
  , mkHeaderRep
  )
where

import "this" Servant.API.Routes.Internal.Header

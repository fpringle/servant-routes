{-# OPTIONS_HADDOCK not-home #-}

{- |
Module      : Servant.API.Routes.Internal.Path
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Internal module, subject to change.
-}
module Servant.API.Routes.Internal.Path
  ( Path (..)
  , renderPath
  , pathSeparator
  )
where

import Data.Aeson
import qualified Data.Text as T

pathSeparator :: T.Text
pathSeparator = "/"

{- | Simple representation of a URL path. The constructor and fields are not exported since I
may change the internal implementation in the future to use 'T.Text' instead of @['T.Text']@.
-}
newtype Path = Path
  { unPath :: [T.Text]
  }
  deriving (Show, Eq, Ord) via [T.Text]

instance ToJSON Path where
  toJSON = toJSON . renderPath

-- | Pretty-print a path, including the leading @/@.
renderPath :: Path -> T.Text
renderPath = (pathSeparator <>) . T.intercalate pathSeparator . unPath

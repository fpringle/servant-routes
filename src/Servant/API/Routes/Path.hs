{- |
Module      : Servant.API.Routes.Path
Copyright   : (c) Frederick Pringle, 2024
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Simple representation of URL paths.
-}
module Servant.API.Routes.Path
  ( Path
  , prependPathPart
  , renderPath
  , rootPath
  )
where

import qualified Data.Text as T
import "this" Servant.API.Routes.Internal.Path

-- | @"/"@
rootPath :: Path
rootPath = Path []

-- | Equivalent to @(:)@.
prependPathPart :: T.Text -> Path -> Path
prependPathPart part (Path parts) =
  Path (splitParts <> parts)
  where
    splitParts = filter (not . T.null) $ T.splitOn pathSeparator part

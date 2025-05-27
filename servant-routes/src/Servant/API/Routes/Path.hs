{- |
Module      : Servant.API.Routes.Path
Copyright   : (c) Frederick Pringle, 2025
License     : BSD-3-Clause
Maintainer  : frederick.pringle@fpringle.com

Simple representation of URL paths.
-}
module Servant.API.Routes.Path
  ( Path
  , prependPathPart
  , prependCapturePart
  , prependCaptureAllPart
  , renderPath
  , rootPath
  )
where

import qualified Data.Text as T
import Data.Typeable
import "this" Servant.API.Routes.Internal.Path
import "this" Servant.API.Routes.Utils

-- | @"/"@
rootPath :: Path
rootPath = Path []

{- | Prepend a simple text path part to an API path.

For example, @prependPathPart "api"@ will transform @\/v2\/users@ to @\/api\/v2\/users@.
-}
prependPathPart :: T.Text -> Path -> Path
prependPathPart part (Path parts) =
  Path (splitParts <> parts)
  where
    splitParts = fmap StringPart . filter (not . T.null) $ T.splitOn pathSeparator part

{- | Prepend a capture path part of a given type to an API path.
Equivalent to @'Servant.API.Capture' name a :>@.

For example, @prependCapturePart \@Int "id"@ will transform @\/detail@ to @\/\<Int>\/detail@.
-}
prependCapturePart ::
  forall a.
  (Typeable a) =>
  T.Text ->
  Path ->
  Path
prependCapturePart name (Path parts) =
  Path (capture : parts)
  where
    capture = CapturePart name $ typeRepOf @a

{- | Prepend a capture-all path part of a given type to an API path.
Equivalent to @'Servant.API.CaptureAll' name a :>@.

For example, @prependCaptureAllPart \@Int "id"@ will transform @\/detail@ to @\/\<[Int]>\/detail@.
-}
prependCaptureAllPart ::
  forall a.
  (Typeable a) =>
  T.Text ->
  Path ->
  Path
prependCaptureAllPart name (Path parts) =
  Path (capture : parts)
  where
    capture = CaptureAllPart name $ typeRepOf @a

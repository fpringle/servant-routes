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
  , PathPart (..)
  )
where

import Data.Aeson
import Data.String
import qualified Data.Text as T
import Data.Typeable

-- | A segment of an API path (between @"/"@s).
data PathPart
  = -- | Just a plain path part, e.g. @"api/"@.
    StringPart T.Text
  | -- | Capture a path part as a variable.
    CapturePart T.Text TypeRep
  | -- | Capture all path part as a list of variables.
    CaptureAllPart T.Text TypeRep
  deriving (Eq, Ord)

instance IsString PathPart where
  fromString = StringPart . T.pack

instance Show PathPart where
  show = T.unpack . renderPathPart

-- | Pretty-print a path part.
renderPathPart :: PathPart -> T.Text
renderPathPart = \case
  StringPart t -> t
  CapturePart _ typ -> "<" <> T.pack (show typ) <> ">"
  CaptureAllPart _ typ -> "<[" <> T.pack (show typ) <> "]>"

-- | Standard path separator @"/"@.
pathSeparator :: T.Text
pathSeparator = "/"

-- | Simple representation of a URL path.
newtype Path = Path
  { unPath :: [PathPart]
  }
  deriving (Eq, Ord) via [PathPart]

instance Show Path where
  show = T.unpack . renderPath

instance ToJSON Path where
  toJSON = toJSON . renderPath

-- | Pretty-print a path, including the leading @/@.
renderPath :: Path -> T.Text
renderPath = (pathSeparator <>) . T.intercalate pathSeparator . fmap renderPathPart . unPath

module Servant.API.Routes.Path
  ( Path
  , prependPathPart
  , renderPath
  , rootPath
  )
where

import Data.Aeson
import qualified Data.Text as T

pathSeparator :: T.Text
pathSeparator = "/"

newtype Path = Path
  { unPath :: [T.Text]
  }
  deriving (Show, Eq, Ord) via [T.Text]

rootPath :: Path
rootPath = Path []

instance ToJSON Path where
  toJSON = toJSON . renderPath

-- | Equivalent to @(:)@.
prependPathPart :: T.Text -> Path -> Path
prependPathPart part (Path parts) =
  Path (splitParts <> parts)
  where
    splitParts = filter (not . T.null) $ T.splitOn pathSeparator part

-- | Pretty-print a path, including the leading @/@.
renderPath :: Path -> T.Text
renderPath = (pathSeparator <>) . T.intercalate pathSeparator . unPath

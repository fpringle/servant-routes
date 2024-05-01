module Servant.API.Routes.PathSpec where

import qualified Data.Text as T
import Servant.API.Routes.Internal.Path
import Servant.API.Routes.Path
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q

genPathPart :: Q.Gen T.Text
genPathPart =
  T.pack <$> do
    first' <- alnum
    rest <- Q.listOf alnumOrOther
    last' <- alnum
    pure $ first' : rest <> [last']
  where
    alnumChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    alnum = Q.elements alnumChars
    alnumOrOther = Q.elements $ alnumChars <> "-_"

genPathParts :: Q.Gen T.Text
genPathParts = unSplit <$> Q.listOf genPathPart

shrinkPathPart :: T.Text -> [T.Text]
shrinkPathPart = fmap T.pack . filter (not . null) . Q.shrinkList (const []) . T.unpack

shrinkPathParts :: T.Text -> [T.Text]
shrinkPathParts = fmap unSplit . Q.shrinkList shrinkPathPart . T.splitOn "/"

unSplit :: [T.Text] -> T.Text
unSplit = mappend pathSeparator . T.intercalate pathSeparator

normalise :: T.Text -> T.Text
normalise =
  unSplit
    . filter (not . T.null)
    . T.splitOn pathSeparator

instance Q.Arbitrary Path where
  arbitrary = Path <$> Q.listOf genPathPart
  shrink = fmap Path . Q.shrinkList shrinkPathPart . unPath

testPrep :: T.Text -> Path -> Q.Property
testPrep part path =
  let preped = prependPathPart part path
      lhs = renderPath preped
      rhs = part <> pathSeparator <> renderPath path
  in  normalise lhs === normalise rhs

spec :: Spec
spec = do
  describe "renderPath" $ do
    it "should render the root path correctly" $
      renderPath rootPath `shouldBe` pathSeparator
  describe "prependPathPart" $ do
    H.prop "should correctly prepend a single path part" $
      \(path :: Path) ->
        Q.forAllShrink genPathPart shrinkPathPart $ \part ->
          testPrep part path
    H.prop "should correctly prepend a multi-part path" $
      \(path :: Path) ->
        Q.forAllShrink genPathParts shrinkPathParts $ \part ->
          testPrep part path

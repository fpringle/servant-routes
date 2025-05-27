module Servant.API.Routes.PathSpec where

import qualified Data.Text as T
import Data.Typeable
import Servant.API.Routes.Internal.Path
import Servant.API.Routes.Path
import Test.Hspec as H
import Test.Hspec.QuickCheck as H
import Test.QuickCheck as Q

genAlphaText :: Q.Gen T.Text
genAlphaText =
  T.pack <$> do
    first' <- alnum
    rest <- Q.listOf alnumOrOther
    last' <- alnum
    pure $ first' : rest <> [last']
  where
    alnumChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    alnum = Q.elements alnumChars
    alnumOrOther = Q.elements $ alnumChars <> "-_"

shrinkText :: T.Text -> [T.Text]
shrinkText = fmap T.pack . filter (not . null) . Q.shrinkList (const []) . T.unpack

genStringPart :: Q.Gen T.Text
genStringPart = genAlphaText

genTypeRep :: Q.Gen TypeRep
genTypeRep =
  Q.elements
    [ typeRep (Proxy @Int)
    , typeRep (Proxy @String)
    , typeRep (Proxy @())
    , typeRep (Proxy @[Int])
    ]

genPathPart :: Q.Gen PathPart
genPathPart =
  Q.frequency
    [ (6, StringPart <$> genStringPart)
    , (3, CapturePart <$> genStringPart <*> genTypeRep)
    , (1, CaptureAllPart <$> genStringPart <*> genTypeRep)
    ]

genStringParts :: Q.Gen T.Text
genStringParts = unSplit <$> Q.listOf genStringPart

shrinkStringPart :: T.Text -> [T.Text]
shrinkStringPart = shrinkText

shrinkStringParts :: T.Text -> [T.Text]
shrinkStringParts = fmap unSplit . Q.shrinkList shrinkStringPart . T.splitOn "/"

unSplit :: [T.Text] -> T.Text
unSplit = mappend pathSeparator . T.intercalate pathSeparator

shrinkPathPart :: PathPart -> [PathPart]
shrinkPathPart = \case
  StringPart str -> StringPart <$> shrinkStringPart str
  CapturePart name tRep -> [CapturePart name' tRep | name' <- shrinkStringPart name]
  CaptureAllPart name tRep -> [CaptureAllPart name' tRep | name' <- shrinkStringPart name]

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
        Q.forAllShrink genStringPart shrinkStringPart $ \part ->
          testPrep part path
    H.prop "should correctly prepend a multi-part path" $
      \(path :: Path) ->
        Q.forAllShrink genStringParts shrinkStringParts $ \part ->
          testPrep part path

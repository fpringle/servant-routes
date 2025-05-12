module Servant.API.Routes.ParamSpec where

import qualified Data.Text as T
import Servant.API.Routes.Param
import Servant.API.Routes.Util
import Test.Hspec as H
import qualified Test.QuickCheck as Q

sing, arrayElem, flag :: Param
sing = singleParam @"sing_sym" @Int
arrayElem = arrayElemParam @"array_sym" @Int
flag = flagParam @"flag_sym"

singExpected, arrayElemExpected, flagExpected :: T.Text
singExpected = "sing_sym=<Int>"
arrayElemExpected = "array_sym=<[Int]>"
flagExpected = "flag_sym"

newtype ParamBasicQInstance = ParamBasicQInstance Param
  deriving (Show, Eq, Ord) via Param

instance Q.Arbitrary ParamBasicQInstance where
  arbitrary = ParamBasicQInstance <$> Q.elements [sing, arrayElem, flag]

spec :: Spec
spec = do
  describe "renderParam" $ do
    it "should render singleParam correctly" $ do
      renderParam sing `shouldBe` singExpected
    it "should render arrayElemParam correctly" $ do
      renderParam arrayElem `shouldBe` arrayElemExpected
    it "should render flagParam correctly" $ do
      renderParam flag `shouldBe` flagExpected

  describe "Hand-rolled instances" $ do
    testEqInstances @ParamBasicQInstance
    testOrdInstances @ParamBasicQInstance

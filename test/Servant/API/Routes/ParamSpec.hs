module Servant.API.Routes.ParamSpec where

import qualified Data.Text as T
import Servant.API.Routes.Param
import Test.Hspec as H

sing, arrayElem, flag :: Param
sing = singleParam @"sing_sym" @Int
arrayElem = arrayElemParam @"array_sym" @Int
flag = flagParam @"flag_sym"

singExpected, arrayElemExpected, flagExpected :: T.Text
singExpected = "sing_sym=<Int>"
arrayElemExpected = "array_sym=<[Int]>"
flagExpected = "flag_sym"

spec :: Spec
spec = do
  describe "renderParam" $ do
    it "should render singleParam correctly" $ do
      renderParam sing `shouldBe` singExpected
    it "should render arrayElemParam correctly" $ do
      renderParam arrayElem `shouldBe` arrayElemExpected
    it "should render flagParam correctly" $ do
      renderParam flag `shouldBe` flagExpected

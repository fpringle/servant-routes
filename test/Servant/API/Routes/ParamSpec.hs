module Servant.API.Routes.ParamSpec where

import qualified Data.Text as T
import Servant.API.Routes.Param
import Test.Hspec as H

sing, arrayElem, flag :: Param
sing = singleParam @"sym" @Int
arrayElem = arrayElemParam @"sym" @Int
flag = flagParam @"sym"

singExpected, arrayElemExpected, flagExpected :: T.Text
singExpected = "sym=<Int>"
arrayElemExpected = "sym=<[Int]>"
flagExpected = "sym"

spec :: Spec
spec = do
  describe "renderParam" $ do
    it "should render singleParam correctly" $ do
      renderParam sing `shouldBe` singExpected
    it "should render arrayElemParam correctly" $ do
      renderParam arrayElem `shouldBe` arrayElemExpected
    it "should render flagParam correctly" $ do
      renderParam flag `shouldBe` flagExpected

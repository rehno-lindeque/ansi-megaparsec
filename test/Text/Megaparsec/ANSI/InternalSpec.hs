module Text.Megaparsec.ANSI.InternalSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.AdHoc

import Text.Megaparsec
import Text.Megaparsec.ANSI.Internal

spec :: Spec
spec =
  describe "psingleton" $ do
    it "works" $ do
      parse (psingleton anySingle :: Parser String) "" "a" `shouldParse` "a"

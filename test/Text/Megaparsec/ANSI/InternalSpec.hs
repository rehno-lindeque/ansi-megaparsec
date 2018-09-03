module Text.Megaparsec.ANSI.InternalSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.AdHoc

import Text.Megaparsec
import Text.Megaparsec.ANSI.Internal

spec :: Spec
spec = do
  describe "psingleton" $ do
    it "works" $ do
      parse (psingleton anySingle :: Parser String) "" "a" `shouldParse` "a"

  describe "pappend" $ do
    it "works" $ do
      parse (chunk "abc" `pappend` chunk "de" :: Parser String) "" "abcdefgh" `shouldParse` "abcde"

  describe "manyTerminatedBy" $ do
    it "works" $ do
      parse (manyTerminatedBy (chunk "ab") (chunk "cd") :: Parser String) "" "abababcdefg" `shouldParse` "abababcd"
      parse (manyTerminatedBy (chunk "ab") (chunk "cd") :: Parser String) "" "cdefg" `shouldParse` "cd"

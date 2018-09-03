module Text.Megaparsec.ANSI.LexerSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.AdHoc

import Text.Megaparsec
import Text.Megaparsec.ANSI.Lexer

spec :: Spec
spec = do
  describe "esc" $ do
    context "when given \\ESC" $
      it "works" $ do
        parse (esc :: Parser Char) "" "\ESC" `shouldParse` '\ESC'
    context "otherwise" $
      it "fails" $ do
        parse (esc :: Parser Char) "" `shouldFailOn` "a"

  describe "anyCsi" $ do
    context "when given ESC+CSI" $
      it "works" $ do
        parse (anyCsi ExcludeSingle8BitC1 :: Parser String) "" "\ESC[" `shouldParse` "\ESC["

    context "when given a single 8-bit CSI" $
      it "works" $ do
        parse (anyCsi IncludeSingle8BitC1 :: Parser String) "" "\x9b" `shouldParse` "\x9b"

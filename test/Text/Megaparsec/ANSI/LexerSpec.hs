module Text.Megaparsec.ANSI.LexerSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.AdHoc

import Text.Megaparsec
import Text.Megaparsec.ANSI.Lexer

spec :: Spec
spec =
  describe "esc" $ do
    context "when given \\ESC" $
      it "works" $ do
        parse (esc :: Parser Char) "" "\ESC" `shouldParse` '\ESC'
    context "otherwise" $
      it "fails" $ do
        parse (esc :: Parser Char) "" `shouldFailOn` "a"


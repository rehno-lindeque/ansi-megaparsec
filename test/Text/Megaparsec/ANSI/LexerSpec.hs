module Text.Megaparsec.ANSI.LexerSpec (spec) where

import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec
import Text.Megaparsec.ANSI.Lexer

type Parser = Parsec Void String

spec :: Spec
spec =
  describe "esc" $ do
    context "when given \\ESC" $
      it "works" $ do
        parse (esc :: Parser Char) "" "\ESC" `shouldParse` '\ESC'
    context "otherwise" $
      it "fails" $ do
        parse (esc :: Parser Char) "" `shouldFailOn` "a"


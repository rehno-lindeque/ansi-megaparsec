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

  describe "escC1" $ do
    context "when given a C1 ESC code"  $
      it "works" $ do
        parse (escC1 :: Parser String) "" "\ESC@" `shouldParse` "\ESC@"
        parse (escC1 :: Parser String) "" "\ESC_" `shouldParse` "\ESC_"

  describe "single8BitC1" $ do
    context "when given a single 8-bit C1" $
      it "works" $ do
        parse (single8BitC1 :: Parser Char) "" "\x80" `shouldParse` '\x80'
        parse (single8BitC1 :: Parser Char) "" "\x9f" `shouldParse` '\x9f'

  describe "anyCsi" $ do
    context "when given ESC+CSI" $
      it "works" $ do
        parse (anyCsi ExcludeSingle8BitC1 :: Parser String) "" "\ESC[" `shouldParse` "\ESC["

    context "when given a single 8-bit CSI" $
      it "works" $ do
        parse (anyCsi IncludeSingle8BitC1 :: Parser String) "" "\x9b" `shouldParse` "\x9b"

  describe "anyAnsiControlSequence" $ do
    context "when given cursor positions" $
      it "works" $ do
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[20;20H" `shouldParse` "\ESC[20;20H"
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[20;20f" `shouldParse` "\ESC[20;20f"
    context "when given a 4-bit SGR color" $
      it "works" $ do
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[31m" `shouldParse` "\ESC[31m"
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[1;31m" `shouldParse` "\ESC[1;31m"
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[39;49m" `shouldParse` "\ESC[39;49m"

    context "when given a 8-bit SGR color" $
      it "works" $ do
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[38;5;206;48;5;57m" `shouldParse` "\ESC[38;5;206;48;5;57m"

    context "when given a 24-bit SGR color" $
      it "works" $ do
        parse (anyAnsiControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[38;2;255;82;197;48;2;155;106;0m" `shouldParse` "\ESC[38;2;255;82;197;48;2;155;106;0m"

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
    context "when given a single 8-bit C1 CSI character as a prefix" $
      it "works" $ do
        parse (anyAnsiControlSequence IncludeSingle8BitC1 :: Parser String) "" "\x9b\&31m" `shouldParse` "\x9b\&31m"
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

  describe "anyControlSequence" $ do
    it "works" $ do
      parse (anyControlSequence ExcludeSingle8BitC1 :: Parser String) "" "\ESC[31m" `shouldParse` "\ESC[31m"
      parse (anyControlSequence IncludeSingle8BitC1 :: Parser String) "" "\x9b\&31m" `shouldParse` "\x9b\&31m"

  describe "anyControlFunction" $ do
    context "given a control sequence" $ do
      it "works" $ do
        parse (anyControlFunction ExcludeSingle8BitC1 :: Parser String) "" "\ESC[31m" `shouldParse` "\ESC[31m"
        parse (anyControlFunction IncludeSingle8BitC1 :: Parser String) "" "\x9b\&31m" `shouldParse` "\x9b\&31m"
    context "given a single control character" $ do
      it "works" $ do
        parse (anyControlFunction ExcludeSingle8BitC1 :: Parser String) "" "\ETX" `shouldParse` "\ETX"
        parse (anyControlFunction IncludeSingle8BitC1 :: Parser String) "" "\x80" `shouldParse` "\x80"

  describe "plainText1" $ do
    it "works" $ do
      parse (plainText :: Parser String) "" "abcdef ghijk\ESC[31m" `shouldParse` "abcdef ghijk"

  describe "plainText1" $ do
    it "works" $ do
      parse (plainText1 :: Parser String) "" "abcdef ghijk\ESC[31m" `shouldParse` "abcdef ghijk"
    it "requires at least 1 plain text character" $ do
      parse (plainText1 :: Parser String) "" `shouldFailOn` "\ESC[31m"

  describe "printText" $ do
    it "works" $ do
      parse (printText :: Parser String) "" "abcdef ghijk\ESC[31m" `shouldParse` "abcdef ghijk"

  describe "printText1" $ do
    it "works" $ do
      parse (printText1 :: Parser String) "" "abcdef ghijk\ESC[31m" `shouldParse` "abcdef ghijk"
      parse (printText1 :: Parser String) "" "abcdef\tghijk\ESC[31m" `shouldParse` "abcdef"
    it "requires at least 1 print character" $ do
      parse (printText1 :: Parser String) "" `shouldFailOn` "\ESC[31m"
      parse (printText1 :: Parser String) "" `shouldFailOn` "\x9b[31m"

  describe "individualChars" $ do
    it "works" $ do
      parse (individualChars :: Parser String) "" "abcdef\nghijk\ESC[31m" `shouldParse` "abcdef\nghijk"
      parse (individualChars1 :: Parser String) "" "\x9b[31m" `shouldParse` "\x9b[31m"

  describe "individualChars1" $ do
    it "works" $ do
      parse (individualChars1 :: Parser String) "" "abcdef\nghijk\ESC[31m" `shouldParse` "abcdef\nghijk"
      parse (individualChars1 :: Parser String) "" "\x9b[31m" `shouldParse` "\x9b[31m"
    it "requires at least 1 print character" $ do
      parse (individualChars1 :: Parser String) "" `shouldFailOn` "\ESC[31m"

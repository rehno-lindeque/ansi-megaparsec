module Main (main) where

import Test.Hspec
import Test.Hspec.Runner

import qualified Text.Megaparsec.ANSISpec
import qualified Text.Megaparsec.ANSI.InternalSpec
import qualified Text.Megaparsec.ANSI.LexerSpec

spec :: Spec
spec = do
  Text.Megaparsec.ANSISpec.spec
  Text.Megaparsec.ANSI.InternalSpec.spec
  Text.Megaparsec.ANSI.LexerSpec.spec

main :: IO ()
main = hspecWith defaultConfig spec

module Main (main) where

import Test.Hspec
import Test.Hspec.Runner

import qualified Text.Megaparsec.ANSISpec
import qualified Text.Megaparsec.ANSI.InternalSpec

spec :: Spec
spec = do
  Text.Megaparsec.ANSISpec.spec
  Text.Megaparsec.ANSI.InternalSpec.spec

main :: IO ()
main = hspecWith defaultConfig spec

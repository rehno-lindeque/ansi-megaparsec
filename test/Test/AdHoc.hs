module Test.AdHoc (Parser) where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String


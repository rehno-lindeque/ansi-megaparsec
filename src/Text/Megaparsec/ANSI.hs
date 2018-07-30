{-# language OverloadedStrings, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Basic parsers for terminal ("ANSI" / ECMA-48 / ISO 6429) escape sequences.


module Text.Megaparsec.ANSI
  ( -- * Escape sequences

    -- $escseq
    escCsi

    -- * C1 codes

    -- $c1codes
  , c1Csi

    -- * C1-compatible parsers (C1 codes + escape sequences)

    -- $c1compat
  , csi
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.ANSI.Internal
import Control.Applicative
import Data.Semigroup (Semigroup, (<>))

-- $escseq
-- Control sequences that are prefixed with an ESC character.

-- | Control sequence introducer
-- This parser recognizes only the 2 character escape-prefixed CSI sequence, not the 8-bit C1 control character.
escCsi :: forall e s m. (MonadParsec e s m, Token s ~ Char, Semigroup (Tokens s)) => m (Tokens s)
escCsi = liftA2 (<>) (singleton esc) (singleton (char '['))

-- $c1codes
-- Note that the ECMA-48 standard allows for certain ESC-prefixed control sequences to be substituted by a single 8-bit
-- C1 control code character (in 8-bit environments). However, according to
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#Escape_sequences Wikipedia>, the 2-byte escape sequences are
-- commonly used in modern environments due to incompatible interpretations in some UTF-8 and CP-1252 character sets.
--
-- For that reason it is recommended to use ESC-prefixed parsers. However, in some rare cases this is not possible
-- the C1 compatible parsers in the next section can be used. Individual c1 character parsers are provided here for
-- completeness only.

-- | Single byte, control sequence introducer
c1Csi :: (MonadParsec e s m, Token s ~ Char) => m Char
c1Csi = char '\x9B'

-- $c1compat
-- Use these parsers when support for single-byte C1 characters is a requirement.

-- | A flag indicating whether C1 control characters need to be parsed. Typically this may be provided to a terminal
-- application via a command-line flag.
--
-- Note that VT100 compatible terminals enable/disable the 8-bit C1 character set using escape sequences called
-- @S8C1T@/@S7C1T@. The parsers provided in this package do not attempt to handle stateful parse modes that this would
-- require. However an application can extend the parsers by wrapping the monad transformer provided by megaparsec,
-- 'Text.Megaparsec.ParsecT', around 'State'.
--
data C1Compatibility
  = ExcludeC1 -- ^ Do not parse 8-bit C1 control codes (similar to @S7C1T@ in VT100-compatible terminals)
  | IncludeC1 -- ^ Parse 8-bit C1 control codes as well as ESC-prefixed alternatives (similar to @S8C1T@ in VT100-compatible terminals)

-- | One- or two-byte CSI sequence
csi :: (MonadParsec e s m, Token s ~ Char, Semigroup (Tokens s)) => C1Compatibility -> m (Tokens s)
csi ExcludeC1 = escCsi <?> "CSI"
csi IncludeC1 = (escCsi <|> singleton c1Csi) <?> "CSI"


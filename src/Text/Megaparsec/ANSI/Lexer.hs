{-# language FlexibleContexts, ScopedTypeVariables #-}
-- | Lexer for splitting terminal ("ANSI" / ECMA-48 / ISO 6429) escape sequences from regular text.
-- For many applications this module may be imported by itself.
module Text.Megaparsec.ANSI.Lexer
  (
    -- * Compatibility

    -- $compat
    Single8BitC1Compatibility (..)

    -- $regtext
  , plainText
  , plainText1
  , printText
  , printText1

    -- * C1 escape sequence variations

    -- $escvar
  , escC1
  , escCsi

    -- * C1 single 8-bit character variations

    -- $singlevar
  , single8BitC1
  , single8BitCsi

    -- * C1 single 8-bit character / escape sequence compatible parsers

    -- $c1compat
  , anyCsi
  , anyAnsiControlSequence

    -- * Trivial parsers

    -- $trivial
  , esc

  ) where

import Text.Megaparsec
import Text.Megaparsec.ANSI.Internal
import Text.Megaparsec.ANSI.Common
import Data.Semigroup (Semigroup, (<>))
import Data.Proxy

-- $compat

-- | A flag indicating whether C1 control character set should be parsed. Typically this may be provided to a terminal
-- application via a command-line flag.
--
-- Note that VT100 compatible terminals enable/disable the 8-bit C1 character set using escape sequences called
-- @S8C1T@/@S7C1T@. The parsers provided in this package do not attempt to handle stateful parse modes that this would
-- require. However an application can extend the parsers by wrapping the monad transformer provided by megaparsec,
-- 'Text.Megaparsec.ParsecT', around 'State'.
data Single8BitC1Compatibility
  = ExcludeSingle8BitC1 -- ^ Recommended: Do not parse 8-bit C1 control codes (similar to @S7C1T@ in VT100-compatible terminals)
  | IncludeSingle8BitC1 -- ^ Parse 8-bit C1 control codes as well as ESC-prefixed alternatives (similar to @S8C1T@ in VT100-compatible terminals)

-- $regtext
-- Use these parsers to match sequences characters that have no special meaning in terminals.

-- | Parse consecutive plain text characters (no control characters other than whitespace).
plainText :: (MonadParsec e s m, Stream s, Ord e, Enum (Token s)) => m (Tokens s)
plainText = takeWhileP Nothing isGraphic
{-# INLINE plainText #-}

-- | Parse consecutive plain text characters (no control characters other than whitespace).
-- Fails if it doesn't match at least 1 character.
plainText1 :: (MonadParsec e s m, Stream s, Ord e, Enum (Token s)) => m (Tokens s)
plainText1 = takeWhile1P Nothing isGraphic
{-# INLINE plainText1 #-}

-- | Parse consecutive print characters (no control characters or whitespace other than the normal space character).
printText :: (MonadParsec e s m, Stream s, Ord e, Enum (Token s)) => m (Tokens s)
printText = takeWhileP Nothing isPrint
{-# INLINE printText #-}

-- | Parse consecutive print characters (no control characters or whitespace other than the normal space character).
-- Fails if it doesn't match at least 1 character.
printText1 :: (MonadParsec e s m, Stream s, Ord e, Enum (Token s)) => m (Tokens s)
printText1 = takeWhile1P Nothing isPrint
{-# INLINE printText1 #-}

-- $escvar
-- Control sequences that have a two character ESC-prefixed variation (as well as an 8-bit variation)

-- | Control sequence introducer
-- This parser recognizes only the two character escape-prefixed CSI sequence, not the 8-bit C1 control character.
-- See ECMA-48, section 5.4.
escCsi :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escCsi =
  -- More simply: chunk "\ESC[" :: IsString s => m (Tokens s)
  -- this just elects to use Enum rather than IsString
  chunk (toChunk 0x1b <> toChunk 0x5b) <?> "ESC_CSI"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escCsi #-}

-- | Escape sequence equivalents to the C1 character set
escC1 :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escC1 = (psingleton esc `pappend` psingleton (satisfy (\c -> c >= toEnum 0x40 && c <= toEnum 0x5F))) <?> "ESC_C1"
{-# INLINE escC1 #-}

-- $singlevar
-- Note that the ECMA-48 standard allows for certain ESC-prefixed control sequences to be substituted by a single 8-bit
-- C1 control code character (in 8-bit environments). However, according to
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#Escape_sequences Wikipedia>, the 2-byte escape sequences are
-- commonly used in modern environments due to incompatible interpretations in some UTF-8 and CP-1252 character sets.
--
-- For that reason it is recommended to use ESC-prefixed codes. However, in some rare instances where this is not possible
-- the compatible parsers in section following this one can be used. Individual c1 character parsers are provided here for
-- the sake of completeness only.

-- | Single character, C1 character set
single8BitC1 :: (MonadParsec e s m, Ord (Token s), Enum (Token s)) => m (Token s)
single8BitC1 = satisfy (\c -> c >= toEnum 0x80 && c <= toEnum 0x9F) <?> "C1"
{-# INLINE single8BitC1 #-}

-- | Single character, control sequence introducer
single8BitCsi :: (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitCsi = single (toEnum 0x9b)
{-# INLINE single8BitCsi #-}

-- $c1compat
-- Use these parsers when support for single-byte C1 characters is a requirement.

-- | Single character or escape sequence, control sequence introducer.
anyCsi :: (MonadParsec e s m, Semigroup (Tokens s), Enum (Token s)) => Single8BitC1Compatibility -> m (Tokens s)
anyCsi ExcludeSingle8BitC1 = escCsi <?> "CSI"
anyCsi IncludeSingle8BitC1 = (escCsi <|> psingleton single8BitCsi) <?> "CSI"
{-# INLINE anyCsi #-}

-- | Any CSI-prefixed control sequence.
anyAnsiControlSequence :: (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => Single8BitC1Compatibility -> m (Tokens s)
anyAnsiControlSequence s8c1Compat = anyCsi s8c1Compat `pappend` parameterBytes `pappend` intermediateBytes `pappend` finalByte
  where
    parameterBytes = takeWhileP (Just "CSI parameter byte") isCsParameterByte
    intermediateBytes = takeWhileP (Just "CSI intermediate byte") isCsIntermediateByte
    finalByte = psingleton (satisfy isCsFinalByte)
{-# INLINE anyAnsiControlSequence #-}

-- $trivial

-- | Escape character, commonly prefixed to escape sequences.
esc :: (MonadParsec e s m, Enum (Token s)) => m (Token s)
esc = single (toEnum 0x1b)
{-# INLINE esc #-}


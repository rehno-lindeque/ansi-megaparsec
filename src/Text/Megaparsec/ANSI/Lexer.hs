{-# language FlexibleContexts, ScopedTypeVariables #-}
-- | Lexer for splitting terminal ("ANSI" / ECMA-48 / ISO 6429) escape sequences from regular text.
-- For many applications this module may be imported by itself.
module Text.Megaparsec.ANSI.Lexer
  (
    -- * Compatibility

    -- $compat
    Single8BitC1Compatibility (..)
  , Iso2022Compatibility (..)

    -- * Regular text

    -- $regtext
  , plainText
  , plainText1
  , printText
  , printText1
  , individualChars
  , individualChars1

    -- * C1 escape sequence variations

    -- $escvar
  , escC1
  , escCsi

    -- * C1 single 8-bit character variations

    -- $singlevar
  , single8BitC1
  , single8BitCsi

    -- * ISO/IEC 2022
    -- $iso2022
  , iso2022CharsetDesignation
  , iso2022AdditionalControlSequence

    -- * C1 single 8-bit character / escape sequence compatible parsers

    -- $c1compat
  , anyCsi
  , anyAnsiControlSequence
  , anyControlSequence
  , anyControlFunction
  , anyCharacterString
  , anyCommandString
  , anyCommandStringDelimeter
  , anyControlString
  , anyControlStringDelimeter

    -- * Trivial parsers

    -- $trivial
  , esc

  ) where

import Text.Megaparsec
import Text.Megaparsec.ANSI.Internal
import Text.Megaparsec.ANSI.Common
import Text.Megaparsec.ANSI.C1
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

-- | A flag indicating whether ISO control characters need to be parsed.
--
-- Note that UTF-8 fits the "other coding system" definition described section 15.4 of ECMA 35. In other words, it exists outside
-- of the ISO 2022 mechanism for shifting character sets.
-- UTF-8 should be prefered when possible since it avoids the various problems inherent to a stateful coding system like ISO 2022.
-- The program @luit@ can be used (and may even be invoked automatically by your terminal emulator) to convert to UTF-8.
-- Use the program @locale@ in order to inspect your environment.
--
-- See also:
-- * https://www.cl.cam.ac.uk/~mgk25/unicode.html#term
-- * https://askubuntu.com/a/133221/17596
-- * https://www.irif.fr/~jch/software/luit
-- * https://invisible-island.net/luit/luit-figures.html
data Iso2022Compatibility
  = ExcludeIso2022 -- ^ Recommended: Do not parse ISO/IEC 2022.
  | IncludeIso2022 -- ^ Parse ISO/IEC 2022

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

-- | Parse consecutive individual characters (including C0 and single 8-bit C1 control characters), but not including escape sequences.
-- This can be used in situations where escape sequences are the only control functions of interest.
individualChars :: (MonadParsec e s m, Stream s, Ord e, Enum (Token s), Eq (Token s)) => m (Tokens s)
individualChars = takeWhileP Nothing (not . isEsc)
{-# INLINE individualChars #-}

-- | Parse consecutive individual characters (including C0 and single 8-bit C1 control characters), but not including escape sequences.
-- This can be used in situations where escape sequences are the only control functions of interest.
-- Fails if it doesn't match at least 1 character.
individualChars1 :: (MonadParsec e s m, Stream s, Ord e, Enum (Token s), Eq (Token s)) => m (Tokens s)
individualChars1 = takeWhile1P Nothing (not . isEsc)
{-# INLINE individualChars1 #-}

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

-- | Matches ISO/IEC 2022 control sequences that designate a character set to use.
-- See ECMA 35, section 13.2.2.
iso2022CharsetDesignation :: (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
iso2022CharsetDesignation =
  psingleton esc `pappend` intermediateBytes `pappend` finalByte
  where
    intermediateBytes = takeWhile1P (Just "ISO/IEC 2022 intermediate byte") isIso2022IntermediateByte
    finalByte = psingleton (satisfy (\c -> isIso2022StandardFinalByte c || isIso2022PrivateFinalByte c))

-- | Additional control functions (escape sequences) unique to ISO/IEC 2022.
-- These are not present in the C0, C1 sets, or any other ANSI escape sequences reserved by ECMA-48.
iso2022AdditionalControlSequence :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
iso2022AdditionalControlSequence =
  psingleton esc `pappend` psingleton (satisfy (\c -> c >= toEnum 0x60 && c <= toEnum 0x7E))
{-# INLINE iso2022AdditionalControlSequence #-}

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

-- | Match any control sequence.
-- Uses lookahead to quickly fail on any @ESC@ (or @CSI@) character prefix.
anyControlSequence :: (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => Single8BitC1Compatibility -> m (Tokens s)
anyControlSequence s8c1Compat =
  anyAnsiControlSequence s8c1Compat <|> escC1
{-# INLINE anyControlSequence #-}

-- | Any terminal control character or sequence of characters.
-- Differs from 'Text.Megaparsec.controlChar' in that multi-byte sequences are fully consumed as lexemes.
anyControlFunction :: (MonadParsec e s m, Stream s, Semigroup (Tokens s), Enum (Token s)) => Single8BitC1Compatibility -> m (Tokens s)
anyControlFunction s8c1Compat =
  (anyControlSequence s8c1Compat <|> psingleton controlChar)
  where
    controlChar = satisfy isControl <?> "control character"
{-# INLINE anyControlFunction #-}

-- | Any control string.
-- See ECMA-48, section 5.6.
anyControlString :: (MonadParsec e s m, Semigroup (Tokens s), Enum (Token s)) => Single8BitC1Compatibility -> m (Tokens s)
anyControlString s8c1Compat =
  anyCharacterString s8c1Compat <|> anyCommandString s8c1Compat
{-# INLINE anyControlString #-}

-- | Any control string delimeter (DCS, SOS, OSC, PM, APC).
anyControlStringDelimeter :: (MonadParsec e s m, Semigroup (Tokens s), Enum (Token s)) => Single8BitC1Compatibility -> m (Tokens s)
anyControlStringDelimeter ExcludeSingle8BitC1 =
  psingleton esc `pappend` psingleton (satisfy (\c -> (c >= toEnum 0x5d && c <= toEnum 0x5f) || c == toEnum 0x50 || c == toEnum 0x58))
anyControlStringDelimeter IncludeSingle8BitC1 =
  anyControlStringDelimeter ExcludeSingle8BitC1 <|> psingleton (satisfy (\c -> (c >= toEnum 0x9d && c <= toEnum 0x9f) || c == toEnum 0x90 || c == toEnum 0x98))
{-# INLINE anyControlStringDelimeter #-}

-- | Any character string (a type of control string).
-- Appears to be recognized, but ignored, by a tested terminal emulator.
--
-- E.g.
--
-- >>> putStrLn "abc\ESCXtest\ESC\\def"
-- abcdef
--
anyCharacterString :: (MonadParsec e s m, Semigroup (Tokens s), Enum (Token s)) => Single8BitC1Compatibility -> m (Tokens s)
anyCharacterString s8c1Compat =
  sos `pappend` manyTerminatedBy (psingleton anySingle) st
  where
    (sos, st) = case s8c1Compat of
      ExcludeSingle8BitC1 ->
        (escSos, escSt)
      IncludeSingle8BitC1 ->
        (escSos <|> psingleton single8BitSos, escSt <|> psingleton single8BitSt)
{-# INLINE anyCharacterString #-}

-- | Any command string delimeter @DCS@, @OSC@, @PM@, @APC@.
anyCommandStringDelimeter :: (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => Single8BitC1Compatibility -> m (Tokens s)
anyCommandStringDelimeter ExcludeSingle8BitC1 =
  psingleton esc `pappend` psingleton (satisfy (\c -> (c >= toEnum 0x5d && c <= toEnum 0x5f) || c == toEnum 0x50))
anyCommandStringDelimeter IncludeSingle8BitC1 =
  anyCommandStringDelimeter ExcludeSingle8BitC1 <|> psingleton (satisfy (\c -> (c >= toEnum 0x9d && c <= toEnum 0x9f) || c == toEnum 0x90))
{-# INLINE anyCommandStringDelimeter #-}

-- | Any command string (a type of control string).
-- * @DCS@ appears to be recognized, but ignored, by a tested terminal emulator.
-- * @PM@ appears to be unrecognized by a tested terminal emulator.
-- * @OSC@ appears to be unrecognized by a tested terminal emulator.
-- * @APC@ appears to be unrecognized by a tested terminal emulator.
--
-- E.g.
--
-- >>> putStrLn "abc\ESCPtest\ESC\\def"
-- abcdef
-- >>> putStrLn "abc\ESC^test\ESC\\def"
-- abc␛^test␛\def
-- >>> putStrLn "abc\ESC]test\ESC\\def"
-- abc␛]test␛\def
-- >>> putStrLn "abc\ESC_test\ESC\\def"
-- abc␛_test␛\def
--
anyCommandString :: (MonadParsec e s m, Semigroup (Tokens s), Enum (Token s)) => Single8BitC1Compatibility -> m (Tokens s)
anyCommandString s8c1Compat =
  anyCommandStringDelimeter s8c1Compat `pappend` takeWhileP (Just "command string byte") isCommandStringByte `pappend` st
  where
    -- We assume that neither UTF-8, nor C1 format effectors are ot accepted since ECMA-48 specifies the ranges explicitly.
    isCommandStringByte c = isAsciiPrint c || isAsciiFormatEffector c
    st = case s8c1Compat of
      ExcludeSingle8BitC1 -> escSt
      IncludeSingle8BitC1 -> escSt <|> psingleton single8BitSt
{-# INLINE anyCommandString #-}

-- $trivial

-- | Escape character, commonly prefixed to escape sequences.
esc :: (MonadParsec e s m, Enum (Token s)) => m (Token s)
esc = single (toEnum 0x1b)
{-# INLINE esc #-}


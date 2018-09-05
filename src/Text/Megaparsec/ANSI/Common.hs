-- | Common helpers for building terminal parsers.
-- Typically there is no need to import this directly as these helpers are generally used building blocks in this package.
module Text.Megaparsec.ANSI.Common
  (
    -- * Trivial matches
    isC0
  , isSp
  , isDel
  , isEsc
  , isPrint
  , isSpace
  , isControl
  , isGraphic

    -- * ASCII specific
  , isAsciiPrint
  , isAsciiFormatEffector

    -- * ECMA 48 control sequence, ancillary bytes
  , isCsParameterByte
  , isCsIntermediateByte
  , isCsFinalByte

    -- * ISO/IEC 2022 character set designation, ancillary bytes
  , isIso2022IntermediateByte
  , isIso2022StandardFinalByte
  , isIso2022PrivateFinalByte
  ) where

import qualified Data.Char as C

-- | Match the C0 character set. Does not include \SP or \DEL.
isC0 :: (Ord char, Enum char) => char -> Bool
isC0 c = c >= toEnum 0 && c <= toEnum 31
{-# INLINE isC0 #-}

-- | Match the \SP (simply @' '@) character.
isSp :: (Eq char, Enum char) => char -> Bool
isSp c = c == toEnum 32
{-# INLINE isSp #-}

-- | Match the \DEL character.
isDel :: (Eq char, Enum char) => char -> Bool
isDel c = c == toEnum 127
{-# INLINE isDel #-}

-- | Match the \ESC character.
isEsc :: (Eq char, Enum char) => char -> Bool
isEsc c = c == toEnum 0x1b
{-# INLINE isEsc #-}

-- | Match the set of print characters.
isPrint :: (Enum char) => char -> Bool
isPrint = C.isPrint . toEnum . fromEnum
{-# INLINE isPrint #-}

-- | Match the set of whitespace characters.
isSpace :: (Enum char) => char -> Bool
isSpace = C.isSpace . toEnum . fromEnum
{-# INLINE isSpace #-}

-- | Match the set of control characters.
isControl :: (Enum char) => char -> Bool
isControl = C.isControl . toEnum . fromEnum
{-# INLINE isControl #-}

-- | Match the set of graphic characters (including whitespace characters in C0 and \160).
isGraphic :: (Enum char) => char -> Bool
isGraphic = (\c -> C.isPrint c || C.isSpace c) . toEnum . fromEnum
{-# INLINE isGraphic #-}

-- | Match the set of printable ASCII characters.
isAsciiPrint :: (Enum char, Ord char) => char -> Bool
isAsciiPrint c = c >= toEnum 0x20 && c <= toEnum 0x7e
{-# INLINE isAsciiPrint #-}

-- | Match format effector characters FE0 to FE5 (@\b@,@\t@,@\n@,@\v@,@\f@,@\r@)
-- Note that this does not include format effectors in the C1 set:
-- In other words, the following are deliberately excluded:
-- @IND@, @NEL@, @HTS@, @HTJ@, @VTS@, @PLD@, @PLU@, @RI@
isAsciiFormatEffector :: (Enum char, Ord char) => char -> Bool
isAsciiFormatEffector c = c >= toEnum 0x8 && c <= toEnum 0xd
{-# INLINE isAsciiFormatEffector #-}

-- | Control sequence parameter byte; zero or more may follow a CSI.
-- See ECMA-48, section 5.4.
isCsParameterByte :: (Enum char, Ord char) => char -> Bool
isCsParameterByte c = c >= toEnum 0x30 && c <= toEnum 0x3f
{-# INLINE isCsParameterByte #-}

-- | Control sequence intermediate byte; zero or more may follow a CSI and any additional control sequence parameter bytes.
-- See ECMA-48, section 5.4.
isCsIntermediateByte :: (Enum char, Ord char) => char -> Bool
isCsIntermediateByte c = c >= toEnum 0x20 && c <= toEnum 0x2f
{-# INLINE isCsIntermediateByte #-}

-- | Control sequence final byte; a single byte that terminates the control sequence.
-- See ECMA-48, section 5.4.
isCsFinalByte :: (Enum char, Ord char) => char -> Bool
isCsFinalByte c = c >= toEnum 0x40 && c <= toEnum 0x7e
{-# INLINE isCsFinalByte #-}

-- | ISO/IEC-2022 intermediate byte; one or more may follow \ESC to designate a character set.
-- See ECMA-35, section 13.2 and table 5 in particular.
isIso2022IntermediateByte :: (Enum char, Ord char) => char -> Bool
isIso2022IntermediateByte c = c >= toEnum 0x20 && c <= toEnum 0x2f
{-# INLINE isIso2022IntermediateByte #-}

-- | ISO/IEC-2022 standard final byte, subject to registration; a single byte that terminates an ISO/IEC 2022 character designating escape sequence.
-- See ECMA-35, section 13.3
isIso2022StandardFinalByte :: (Enum char, Ord char) => char -> Bool
isIso2022StandardFinalByte c = c >= toEnum 0x40 && c <= toEnum 0x7f
{-# INLINE isIso2022StandardFinalByte #-}

-- | ISO/IEC-2022 final byte reserved for private use; a single byte that terminates an ISO/IEC 2022 character designating escape sequence.
-- See ECMA-35, section 13.3
isIso2022PrivateFinalByte :: (Enum char, Ord char) => char -> Bool
isIso2022PrivateFinalByte c = c >= toEnum 0x30 && c <= toEnum 0x3F
{-# INLINE isIso2022PrivateFinalByte #-}

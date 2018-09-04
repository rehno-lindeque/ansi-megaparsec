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

    -- * ECMA 48 control sequence, ancillary bytes
  , isCsParameterByte
  , isCsIntermediateByte
  , isCsFinalByte
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

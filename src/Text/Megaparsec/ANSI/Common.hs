-- | Common helpers for building terminal parsers.
-- Typically there is no need to import this directly as these helpers are generally used building blocks in this package.
module Text.Megaparsec.ANSI.Common
  (
    -- * ECMA 48 control sequence, ancillary bytes
    isCsParameterByte
  , isCsIntermediateByte
  , isCsFinalByte
  ) where

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

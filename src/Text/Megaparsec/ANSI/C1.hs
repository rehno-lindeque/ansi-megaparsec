{-# language DeriveGeneric, FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Text.Megaparsec.ANSI.C1
  ( C1 (..)

    -- * C1 escape sequence variations

  , escSos
  , escSt
  , escDcs
  , escSos
  , escSt
  , escOsc
  , escPm
  , escApc
  , single8BitDcs
  , single8BitSos
  , single8BitSt
  , single8BitOsc
  , single8BitPm
  , single8BitApc

    -- * C1 single 8-bit character variations
  , single8BitSos
  , single8BitSt

  ) where

import Text.Megaparsec
import GHC.Generics (Generic)
import Data.Proxy
import Data.Semigroup (Semigroup, (<>))

data C1
  = PAD -- Padding Character
  | HOP -- High Octet Preset
  | BPH -- Break Permitted Here
  | NBH -- No Break Here
  | IND -- Index
  | NEL -- Next Line
  | SSA -- Start of Selected Area
  | ESA -- End of Selected Area
  | HTS -- Character Tabulation Set
  | HTJ -- Character Tabulation With Justification
  | VTS -- Line Tabulation Set
  | PLD -- Partial Line Forward
  | PLU -- Partial Line Backward
  | RI  -- Reverse Line Feed
  | SS2 -- Single-Shift 2
  | SS3 -- Single-Shift 3
  | DCS -- Device Control String
  | PU1 -- Private Use 1
  | PU2 -- Private Use 2
  | STS -- Set Transmit State
  | CCH -- Cancel character
  | MW  -- Message Waiting
  | SPA -- Start of Protected Area
  | EPA -- End of Protected Area
  | SOS -- Start of String
  | SGC -- Single Graphic Character Introducer
  | SCI -- Single Character Introducer
  | CSI -- Control Sequence Introducer
  | ST  -- String Terminator
  | OSC -- Operating System Command
  | PM  -- Privacy Message
  | APC -- Application Program Command
  deriving (Eq, Ord, Bounded, Show, Read, Generic)
  -- TODO: enum

escDcs :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escDcs = chunk (toChunk 0x1b <> toChunk 0x50) <?> "ESC_DCS"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escDcs #-}

escSos :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escSos = chunk (toChunk 0x1b <> toChunk 0x58) <?> "ESC_SOS"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escSos #-}

escSt :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escSt = chunk (toChunk 0x1b <> toChunk 0x5c) <?> "ESC_ST"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escSt #-}

escOsc :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escOsc = chunk (toChunk 0x1b <> toChunk 0x5d) <?> "ESC_OSC"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escOsc #-}

escPm :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escPm = chunk (toChunk 0x1b <> toChunk 0x5e) <?> "ESC_PM"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escPm #-}

escApc :: forall e s m. (MonadParsec e s m, Enum (Token s), Semigroup (Tokens s)) => m (Tokens s)
escApc = chunk (toChunk 0x1b <> toChunk 0x5f) <?> "ESC_APC"
  where
    toChunk c = tokenToChunk (Proxy :: Proxy s) (toEnum c)
{-# INLINE escApc #-}

single8BitDcs :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitDcs = single (toEnum 0x90)
{-# INLINE single8BitDcs #-}

single8BitSos :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitSos = single (toEnum 0x98)
{-# INLINE single8BitSos #-}

single8BitSt :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitSt = single (toEnum 0x9c)
{-# INLINE single8BitSt #-}

single8BitOsc :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitOsc = single (toEnum 0x9d)
{-# INLINE single8BitOsc #-}

single8BitPm :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitPm = single (toEnum 0x9e)
{-# INLINE single8BitPm #-}

single8BitApc :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitApc = single (toEnum 0x9f)
{-# INLINE single8BitApc #-}


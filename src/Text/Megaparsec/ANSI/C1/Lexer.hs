{-# language FlexibleContexts, ScopedTypeVariables #-}
module Text.Megaparsec.ANSI.C1.Lexer
  (
    -- * C1 escape sequence variations

    escDcs
  , escSos
  , escSt
  , escOsc
  , escPm
  , escApc

    -- * C1 single 8-bit character variations

  , single8BitDcs
  , single8BitSos
  , single8BitSt
  , single8BitOsc
  , single8BitPm
  , single8BitApc

  ) where

import Text.Megaparsec
import Data.Proxy
import Data.Semigroup (Semigroup, (<>))

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

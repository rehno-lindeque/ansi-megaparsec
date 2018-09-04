{-# language FlexibleContexts, ScopedTypeVariables #-}
module Text.Megaparsec.ANSI.C1
  (
    -- * C1 escape sequence variations

    escSos
  , escSt

    -- * C1 single 8-bit character variations
  , single8BitSos
  , single8BitSt

  ) where

import Text.Megaparsec
import Data.Proxy
import Data.Semigroup (Semigroup, (<>))

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

single8BitSos :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitSos = single (toEnum 0x98)
{-# INLINE single8BitSos #-}

single8BitSt :: forall e s m. (MonadParsec e s m, Enum (Token s)) => m (Token s)
single8BitSt = single (toEnum 0x9c)
{-# INLINE single8BitSt #-}

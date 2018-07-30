{-# language OverloadedStrings, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Internal helpers for building terminal parsers
module Text.Megaparsec.ANSI.Internal
  (
    -- * Helpers

    singleton

    -- * Trivial parsers

  , esc

  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
-- import Data.Semigroup (Semigroup, (<>))
import Data.Proxy (Proxy (..))

singleton :: forall e s m. (Stream s, MonadParsec e s m) => m (Token s) -> m (Tokens s)
singleton = fmap (tokenToChunk (Proxy :: Proxy s))

-- | Escape character, commonly prefixed to escape sequences.
esc :: (MonadParsec e s m, Token s ~ Char) => m Char
esc = char '\ESC'


{-# language FlexibleContexts #-}
-- | Lexer for splitting terminal ("ANSI" / ECMA-48 / ISO 6429) escape sequences from regular text.
-- For many applications this module may be imported by itself.
module Text.Megaparsec.ANSI.Lexer
  (
    -- * Trivial parsers

    -- $trivial
    esc

  ) where

import Text.Megaparsec
import Text.Megaparsec.ANSI.Internal

-- $trivial

-- | Escape character, commonly prefixed to escape sequences.
esc :: (MonadParsec e s m, Enum (Token s)) => m (Token s)
esc = single (toEnum 0x1B)
{-# INLINE esc #-}


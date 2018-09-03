{-# language OverloadedStrings, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Internal helpers for building terminal parsers
module Text.Megaparsec.ANSI.Internal
  ( psingleton
  ) where

import Text.Megaparsec
import Data.Proxy (Proxy (..))



-- | Turn a character parser into a string parser
psingleton :: forall e s m. (Stream s, MonadParsec e s m) => m (Token s) -> m (Tokens s)
psingleton = fmap (tokenToChunk (Proxy :: Proxy s))
{-# INLINE psingleton #-}

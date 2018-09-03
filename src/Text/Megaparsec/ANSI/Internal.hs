{-# language OverloadedStrings, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Internal helpers that normally do not need to be imported.
module Text.Megaparsec.ANSI.Internal
  ( psingleton
  , pappend
  , manyTerminatedBy
  ) where

import Text.Megaparsec
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup, (<>))
import Control.Applicative (liftA2)

-- | Turn a character parser into a string parser
psingleton :: forall e s m. (Stream s, MonadParsec e s m) => m (Token s) -> m (Tokens s)
psingleton = fmap (tokenToChunk (Proxy :: Proxy s))
{-# INLINE psingleton #-}

-- | Append two parse results without requiring @Semigroup (m a)@
pappend :: (MonadParsec e s m, Semigroup a) => m a -> m a -> m a
pappend = liftA2 (<>)
{-# INLINE pappend #-}

-- | Similar to 'manyTill', but includes the terminating chunk (and the parse results are semigroup).
manyTerminatedBy :: (MonadParsec e s m, Semigroup a) => m a -> m a -> m a
manyTerminatedBy p end = go
  where
    go = end <|> pappend p go
{-# INLINE manyTerminatedBy #-}

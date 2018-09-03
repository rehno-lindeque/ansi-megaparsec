{-# language ScopedTypeVariables, FlexibleContexts #-}

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

-- -- | Helper for defining prefixed (typically escape-prefixed in this package) without paying any additional backtracking cost
-- -- These are mainly intended to be used with 'Control.Alternative.(<|>)' where the common prefixes can be shared.
-- -- The various parsers don't really make sense without their ESC prefixes, so this makes things a little bit more clear.
-- newtype PrefixedParsecT e s m (prefix :: Symbol) = PrefixedParsecT { unPrefixed :: ParsecT e s m (Tokens s) }

-- instance (Stream s, Semigroup (Tokens s)) => Semigroup (PrefixedParsecT e s m prefix) where
--   x <> y = PrefixedParsecT (unPrefixed x <> unPrefixed y)

-- fromPrefixed :: (IsString (Tokens s), KnownSymbol prefix, Stream s, Ord e) => PrefixedParsecT e s m prefix -> ParsecT e s m (Tokens s)
-- fromPrefixed (prefixedParser :: KnownSymbol prefix => PrefixedParsecT e s m prefix) = tokens (==) (fromString (symbolVal' (proxy# :: KnownSymbol prefix => Proxy# prefix)))
-- -- fromPrefixed prefixedParser = tokens (==) (fromString (symbolVal' (proxy# :: KnownSymbol prefix => Proxy# prefix))) <> unPrefixed (prefixedParser :: KnownSymbol prefix => Prefixed e s m prefix)
-- -- fromPrefixed :: (IsString (Tokens s), Stream s, Ord e) => (forall prefix. KnownSymbol prefix => Prefixed e s m prefix) -> ParsecT e s m (Tokens s)
-- -- fromPrefixed prefixedParser = tokens (==) (fromString (symbolVal' (proxy# :: KnownSymbol prefix => Proxy# prefix)))
-- {-# INLINE fromPrefixed #-}

-- prefixed :: forall e s m (prefix :: Symbol). KnownSymbol prefix => Proxy# prefix -> ParsecT e s m (Tokens s) -> PrefixedParsecT e s m prefix
-- prefixed _ parser = Prefixed parser

-- escA :: (Token s ~ Char) => PrefixedParsecT e s m "\ESC"
-- escA = prefixed proxy# k

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

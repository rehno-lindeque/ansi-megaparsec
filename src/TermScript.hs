{-# language DeriveFunctor, LambdaCase, OverloadedStrings #-}
{-|
    Basic TermScript types and a simple one-off implementation which can be used for simple send & parse scripts that don't
    require any external intervention.

    See 'InterativeTermScript' for more flexible scripts that can be interleaved with miscelaneous effectful commands.
-}

module TermScript
  ( TermScript
  , TermScriptT
  , TermInteraction (..)
  , putKey
  , readPlain
  ) where

import Data.Text (Text)
-- import qualified Data.Text as Text
-- import qualified Data.Text.Encoding as Text
-- import Data.Semigroup
-- import Data.Functor.Identity (Identity, runIdentity)
-- import Control.Monad.Trans.Free
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class (lift)
-- import qualified Graphics.Vty.Input as Vty
-- import qualified System.Posix.Pty as Pty
import Control.Monad.Trans.Except (ExceptT)

-- * DSL

data TermInteraction cont
  = PutKey [Vty.Modifier] Vty.Key cont
  | Read (Text -> cont)
  | Fail
  | Done
  deriving Functor


type TermScriptT m result = FreeT TermInteraction m result
type TermScript result = TermScriptT (ExceptT Text Identity) result -- This is @Free TermInteration result@ with exceptions

-- * Basic scripts

putKey :: Monad m => [Vty.Modifier] -> Vty.Key -> TermScriptT m ()
putKey modifiers key = liftF (PutKey modifiers key ())

readPlain :: Monad m => TermScriptT m Text
readPlain = liftF (Read id)


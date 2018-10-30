{-# language DeriveFunctor, LambdaCase, OverloadedStrings, FlexibleInstances, RankNTypes, ScopedTypeVariables, KindSignatures #-}

-- | The interpreter for InteractivePtyScript provides an implementation
--   that can be used to simultaneously run the underlying program
--   while also being able to issue scripted commands.
--
--   Use this when you want to pipe pty output to the screen and proxy user input back to the program while
--   simultaneously triggering complex scripted commands.

module InteractivePtyScript
  ( InteractivePtyScript
  , PtyScriptError(..)
  , PtyScriptState(..)
  , run
  , runForever
  , interpret
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Semigroup
import Data.Functor.Identity (runIdentity)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
-- import Control.Monad.Catch
import qualified Control.Concurrent as Concurrent (ThreadId, threadDelay, forkFinally, killThread, MVar, newMVar, newEmptyMVar, tryTakeMVar, takeMVar, putMVar, swapMVar, modifyMVar_)
import Control.Concurrent (MVar)
import qualified Control.Exception as Exception (mask)
import Control.Exception (onException)
import System.Posix.Pty (Pty, threadWaitWritePty, threadWaitReadPty, tryReadPty, writePty)
import qualified System.Posix.Pty as Pty
import qualified PtyScript
import PtyScript (PtyScript, PtyScriptT, PtyInteraction(..))
import qualified Data.Attoparsec.Text as Attoparsec
import PtyParser (parseSegment)
import KeyboardInput (translateKeyEvent)


-- * Types

type InteractiveT io = ReaderT Pty (StateT PtyScriptState io)
type InteractivePtyIO = ExceptT PtyScriptError (InteractiveT IO)
type InteractivePtyScript result = PtyScriptT InteractivePtyIO result


-- ??? type InteractivePtyIO = ExceptT Text (ReadT (IORef PtyScriptState) IO)

-- TODO: refine
data PtyScriptError = Fatal Text | NonFatal Text

-- * State

data PtyScriptState = PtyScriptState
  { parserState :: Attoparsec.IResult Text ([Text], Text)
  }

initialState :: PtyScriptState
initialState = PtyScriptState
  { parserState = Attoparsec.Done "" ([],"")
  }

-- * Interpreter

-- runTerminal :: ProcessHandle -> Pty -> (([Text], Text) -> IO ()) -> IO ()
-- runTerminal handle pty f =
--   runPtyParser . whileProcessPty handle pty $ do
--     eoutput <- lift (tryReadPty pty)
--     case eoutput of
--       Left code ->
--         lift (Text.putStrLn ("debug (pty control code): " <> tshow code))
--       Right output -> do
--         withPtyOutput output f
--   where
--     tshow :: Show a => a -> Text
--     tshow = Text.pack . show
--     whileProcessPty handle pty io =
--       let io' = do
--             lift (threadWaitReadPty pty)
--             mexitCode <- lift (getProcessExitCode handle)
--             when (mexitCode == Nothing) (io >> io')
--       in io'

run :: Pty -> InteractivePtyScript x -> IO (Either PtyScriptError x, PtyScriptState)
run pty script = runStateT (runReaderT (runExceptT (iterT interpret script)) pty) initialState

-- Run the script in a MonadIO context of your choosing while also keeping the script's environment in tact
runInteractive :: MonadIO m => InteractivePtyScript x -> InteractiveT m (Either PtyScriptError x)
runInteractive script = (mapReaderT  . mapStateT) liftIO (runExceptT (iterT interpret script))

-- runForever $ \run ->
--   a <- getMVar sdf
--   if a then run scriptA else run scriptB

-- hoistPtyScriptIO

-- Run forever without losing track of state
runForever :: MonadIO m
  => Pty
  -> ((forall x. InteractivePtyScript x -> InteractiveT m (Either PtyScriptError x)) -> InteractiveT m a)
  -> m (b, PtyScriptState)
runForever pty f = runStateT (runReaderT (forever (f run')) pty) initialState
  where
    run' script = runInteractive script

-- bracket :: IO a -> InteractivePtyScript x -> IO (Either PtyScriptError x, PtyScriptState)
-- bracket pty ptyMutex script = runReaderT (runStateT (runExceptT (iterT interpret script)) initialState) pty

-- mask :: forall b. ((forall a. InteractivePtyScript a -> InteractivePtyScript a) -> IO b) -> InteractivePtyScript b
-- mask inner = do
--   startState <- (lift . lift) get
--   pty <- (lift . lift . lift) ask
--   let inner' :: (forall a. IO a -> IO a) -> IO b
--       inner' = \restore ->
--         let
--           restore' script = do
--             result <- lift . lift . lift . lift $ restore (runReaderT (runStateT (runExceptT (iterT interpret script)) startState) pty)
--             lift (ExceptT (return (fst result)))
--         in
--           inner restore'
--   result <- lift . lift . lift . lift $ Exception.mask inner'
--   return result


  --state <- get
  --runStateT (runExceptT (iterT interpret (io restore))) state

-- mask :: forall b. ((forall a. InteractivePtyScript a -> IO (Either PtyScriptError a)) -> IO b) -> InteractivePtyScript b
-- mask inner = do
--   startState <- (lift . lift) get
--   pty <- (lift . lift . lift) ask
--   let inner' :: (forall a. IO a -> IO a) -> IO b
--       inner' = \restore ->
--         let
--           restore' script = do
--             result <- restore (runReaderT (runStateT (runExceptT (iterT interpret script)) startState) pty)
--             return (fst result)
--         i
--           inner restore'
--   result <- lift . lift . lift . lift $ Exception.mask inner'
--   return result

-- onException :: InteractivePtyScript a -> IO b -> InteractivePtyScript a
-- onException = do
--   startState <- (lift . lift) get
--   pty <- (lift . lift . lift) ask
--   result <- (runReaderT (runStateT (runExceptT (iterT interpret script)) startState) pty)

-- runInIO :: InteractivePtyScript a -> (a -> IO b) -> InteractivePtyScript b
-- runInIO script io = do
--   startState <- (lift . lift) get
--   pty <- (lift . lift . lift) ask
--   (b, endState) <- runReaderT (runStateT (runExceptT (iterT interpret script)) startState) pty >>= \(ea, state) ->
--     case ea of
--       Left e -> io _
--       Right a -> io a
--     return (ea,state)
--   put endState
--   return b

-- withMVar :: MVar a -> (a -> InteractivePtyScript b) -> InteractivePtyScript b
-- withMVar m script =
--   mask $ \restore -> do
--     a <- Concurrent.takeMVar m
--     b <- restore (script a) `onException` putMVar m a
--     putMVar m a
--     return b

-- continue :: Pty -> InteractivePtyScript x -> IO (Either PtyScriptError x, PtyScriptState)
-- continue state pty script = runStateT (runExceptT (iterT (interpret pty) script)) state
--

interpret :: PtyInteraction (InteractivePtyIO result) -> InteractivePtyIO result
interpret = go
  where
    go :: PtyInteraction (InteractivePtyIO result) -> InteractivePtyIO result
    go = \case
      Fail ->
        throwE (NonFatal "Script failed")
      Done ->
        error "TODO"
      PutKey modifiers key cont -> do
        case translateKeyEvent modifiers key of
          Nothing -> do
            throwE (NonFatal ("Unknown key: " <> (Text.pack . show) key <> ", modifiers: " <> (Text.pack . show) modifiers))
          Just keycode -> do
            pty <- lift ask
            lift . lift . lift $ do
              threadWaitWrite (getFd pty)
              writePty pty keycode
              Pty.drainOutput pty
        cont
      Read f -> do
        pty <- lift ask
        eoutput <- lift . lift . lift $ do
          threadWaitRead (getFd pty)
          tryReadPty pty
        case eoutput of
          Left e ->
            throwE (Fatal ("Unkown error reading pty: " <> (Text.pack . show) e))
          Right output -> do
            let outputText = Text.decodeUtf8 output
            lift . lift $ modify'
              (\state -> state {
                parserState = case parserState state of
                                Attoparsec.Partial f -> f outputText
                                _ -> parseSegment outputText
              })
            f outputText


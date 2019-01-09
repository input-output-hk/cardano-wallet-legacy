module Cardano.Wallet.Client.CLI.ProcessUtil.POSIX
  ( checkProcessExists'
  , ProcessID
  , cancelOnExit
  , interruptSelf
  ) where

import           Universum

import           Control.Concurrent.Async (Async (..), cancel)
import           Control.Exception (IOException)

import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types

checkProcessExists' :: ProcessID -> IO Bool
checkProcessExists' pid = hasGroup `catch` (\(_ :: IOException) -> pure False)
  where
    hasGroup = (> 0) <$> getProcessGroupIDOf pid

-- | Installs sigterm and sigint handlers for while an IO action is
-- being run. The signal handlers cancel an Async process.
cancelOnExit :: Async a -> IO b -> IO b
cancelOnExit a = cancelOnCtrlC a . cancelOnTerm a
  where
    addHandler sig h = installHandler sig h Nothing
    cancelOnSig sig as act = bracket (addHandler sig (Catch (cancel as))) (addHandler sig) (const act)
    cancelOnCtrlC = cancelOnSig keyboardSignal
    cancelOnTerm = cancelOnSig softwareTermination

-- | This is only used for testing.
interruptSelf :: IO ()
interruptSelf = raiseSignal sigINT

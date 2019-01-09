module Cardano.Wallet.Client.CLI.ProcessUtil.Windows
    ( checkProcessExists'
    , ProcessID
    , cancelOnExit
    , interruptSelf
    ) where

import           Universum

import           Control.Concurrent.Async (Async (..), cancel)
import           Control.Exception (IOException)

import           System.Win32.Process (ProcessId, getCurrentProcessId,
                     openProcess, pROCESS_VM_READ, terminateProcessById)
import           System.Win32.Types (nullHANDLE)

-- TODO: Implement for Windows. This code has not been tested yet.

type ProcessID = ProcessId

checkProcessExists' :: ProcessID -> IO Bool
checkProcessExists' pid = canOpen `catch` (\(_ :: IOException) -> pure False)
  where
    canOpen = (/= nullHANDLE) <$> openProcess pROCESS_VM_READ False pid

cancelOnExit :: Async a -> IO b -> IO b
cancelOnExit _ = id

interruptSelf :: IO ()
interruptSelf = getCurrentProcessId >>= terminateProcessById

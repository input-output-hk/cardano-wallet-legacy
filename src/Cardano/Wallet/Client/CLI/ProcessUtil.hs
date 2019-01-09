{-# LANGUAGE CPP #-}

module Cardano.Wallet.Client.CLI.ProcessUtil
    ( checkProcessExists
    , ProcessID
    , cancelOnExit
    , interruptSelf
    ) where

import           Universum

#if defined(mingw32_HOST_OS)
import           Cardano.Wallet.Client.CLI.ProcessUtil.Windows
#else
import           Cardano.Wallet.Client.CLI.ProcessUtil.POSIX
#endif

-- | Monitors that a given process is still running. Useful for when
-- polling the API of that process.
checkProcessExists :: Maybe ProcessID -> IO Bool
checkProcessExists Nothing    = pure True
checkProcessExists (Just pid) = checkProcessExists' pid

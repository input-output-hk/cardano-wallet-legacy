module Cardano.Wallet.Client.CLI.Run where

import           Criterion.Measurement (secs)
import           Data.Aeson (ToJSON (..), encodeFile)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.IO as T
import           Formatting (sformat, shown, string, (%))

import           System.Exit (ExitCode (..))
import           Universum

import           Cardano.Wallet.Client.Easy
import           Cardano.Wallet.Client.CLI.Options


----------------------------------------------------------------------------
-- Main Program

runAction :: Action IO -> WalletClient IO -> IO ExitCode
runAction act wc = case act of
  WaitForSync mpid out -> waitForSync (waitOptionsPID mpid) wc >>= handleWaitResult out
  WaitForRestore mpid out -> waitForRestore (waitOptionsPID mpid) wc >>= handleWaitResult out
  WalletEndpoint req -> req wc >>= printStatus
  WalletEndpointVoid req -> req wc >>= printStatusVoid

printStatus :: ToJSON a => Either ClientError a -> IO ExitCode
printStatus = printStatus' . fmap Just

printStatusVoid :: Either ClientError () -> IO ExitCode
printStatusVoid = printStatus' . fmap (const (Nothing :: Maybe ()))

printStatus' :: ToJSON a => Either ClientError (Maybe a) -> IO ExitCode
printStatus' resp = case resp of
  Right Nothing -> pure ExitSuccess
  Right (Just a) -> L8.putStrLn (encodePretty a) >> pure ExitSuccess
  Left cerr -> (T.hPutStrLn stderr $ sformat ("client error: "%shown) cerr) >> pure (ExitFailure 100)

-- | Convert the sync result into a goodbye message, print it, write
-- the output file, and return an appropriate program exit status.
handleWaitResult :: ToJSON r => Maybe FilePath -> SyncResult r -> IO ExitCode
handleWaitResult mout res@(SyncResult err start dur _) = do
  putStrLn (msg err)
  putStrLn $ sformat ("Started: "%shown) start
  putStrLn $ sformat ("Elapsed time: "%string) (secs dur)
  whenJust mout (writeJSON res)
  pure (code err)
  where
    msg :: Maybe SyncError -> Text
    msg = maybe "Finished" show

    code Nothing                         = ExitSuccess
    code (Just (SyncErrorClient _))      = ExitFailure 1
    code (Just (SyncErrorProcessDied _)) = ExitFailure 2
    code (Just (SyncErrorTimedOut _))    = ExitFailure 3
    code (Just (SyncErrorException _))   = ExitFailure 4
    code (Just (SyncErrorInterrupted))   = ExitSuccess

    writeJSON sr f = do
      putStrLn $ sformat ("Writing output to "%shown) f
      encodeFile f sr

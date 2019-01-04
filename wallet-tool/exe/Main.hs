module Main where

import           Options.Applicative
import           System.Exit (ExitCode)
import           Universum

import           Cardano.Wallet.Client.CLI
import           Cardano.Wallet.Client.Easy

main :: IO ()
main = exitWith =<< uncurry run =<< execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc <> progDesc docs )

run :: ConnectConfig -> Action IO -> IO ExitCode
run cfg act = walletClientFromConfig cfg >>= runAction act

docs :: String
docs = "Connect to the API of a running Cardano Wallet backend."

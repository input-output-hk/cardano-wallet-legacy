{-# LANGUAGE GADTs #-}

module Cardano.Wallet.Client.CLI.Options where

import           Data.Aeson (ToJSON)
import qualified Data.Attoparsec.Text as A
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Options.Applicative
import qualified Pos.Client.Txp.Util as Core
import qualified Pos.Core as Core
import           Pos.Core.NetworkAddress (addrParserNoWildcard)
import           Servant.Client (BaseUrl (..), Scheme (Https))
import qualified Text.Parsec as Parsec
import           Universum

import           Cardano.Mnemonic (mkMnemonic)
import           Cardano.Wallet.API.V1.Types (Account (..), AccountIndex,
                     AssuranceLevel (..), BackupPhrase (..),
                     EstimatedFees (..), NewWallet (..), Payment (..),
                     PaymentDistribution (..), PaymentSource (..),
                     Transaction (..), UtxoStatistics, WalAddress (..),
                     WalletAddress (..), WalletCoin (..), WalletId (..),
                     WalletInputSelectionPolicy (..),
                     WalletInputSelectionPolicy (..), WalletOperation (..),
                     WalletPassPhrase (..), mkAccountIndex, mkSpendingPassword)
import           Cardano.Wallet.Client (getAccounts, getAccountAddressIndex,
                     getAddressIndex, getTransactionIndex, getWallets)
import           Cardano.Wallet.Client.Easy
import           Cardano.Wallet.Client.CLI.ProcessUtil (ProcessID)

----------------------------------------------------------------------------
-- CLI Types

-- | The command from the user
data Action m where
    WaitForSync :: Maybe ProcessID -> Maybe FilePath -> Action m
    WaitForRestore :: Maybe ProcessID -> Maybe FilePath -> Action m
    -- | Wallet API call
    WalletEndpoint :: forall m a. ToJSON a => WalletCall m a -> Action m
    -- | Wallet API call with empty response
    WalletEndpointVoid :: WalletCallVoid m -> Action m

-- | An API call
type WalletCall m a = WalletClient m -> Resp m a

-- | An API call with no data in the response
type WalletCallVoid m = WalletClient m -> m (Either ClientError ())

----------------------------------------------------------------------------
-- Option parsers

optionsParser :: Parser (ConnectConfig, Action IO)
optionsParser = (,) <$> connectConfigP <*> actionP

connectConfigP :: Parser ConnectConfig
connectConfigP = certConfigP <*> authenticateServerP <*> baseUrlP
  where
    certConfigP = stateDirP <|> certFilesP
    stateDirP   = stateDirConnectConfig <$> strOption
                  (long "state-dir"
                   <> metavar "DIRECTORY"
                   <> help "Wallet state directory containing \"tls\" subdirectory with certificates")
    certFilesP  = ConnectConfig
        <$> optional (clientAuthCertKeyP <|> clientAuthPemP)
        <*> optional (strOption
                    (long "cacert"
                     <> metavar "FILENAME"
                     <> help "CA certificate chain for authenticating the server"))

clientAuthCertKeyP :: Parser ClientAuthConfig
clientAuthCertKeyP = ClientAuthConfig
    <$> strOption (long "cert"
                   <> metavar "FILENAME"
                   <> help "X509 certificate file")
    <*> strOption (long "key"
                   <> metavar "FILENAME"
                   <> help "Certificate key file")

clientAuthPemP :: Parser ClientAuthConfig
clientAuthPemP = pemConfig <$> strOption (long "pem"
                                          <> metavar "FILENAME"
                                          <> help "Combined X509 certificate and key file")
  where
    pemConfig pem = ClientAuthConfig pem pem

baseUrlP :: Parser BaseUrl
baseUrlP = baseUrl <$> addrP <*> path
  where
    baseUrl (host, port) = BaseUrl Https (B8.unpack host) (fromIntegral port)
    addrP = argument readAddrM (metavar "HOST:PORT" <> value ("localhost", 8090) <> help "Wallet API host and port to connect to")
    readAddrM = eitherReader (first show . Parsec.parse addrParserNoWildcard "" . T.pack)
    path = strOption (long "path" <> short 'p' <> value "" <> metavar "PATH" <> help "Base URL path")

authenticateServerP :: Parser AuthenticateServer
authenticateServerP = flag AllowInsecure AuthenticateServer
                      ( long "insecure"
                      <> short 'k'
                      <> help "Skip server certificate authentication" )

----------------------------------------------------------------------------
-- Action parsers

actionP :: Monad m => Parser (Action m)
actionP = hsubparser
    ( command "wait-for-sync" (info waitForSyncP (progDesc "Poll wallet until it has fully synced its chain"))
      <> command "wait-for-restore" (info waitForRestoreP (progDesc "Poll wallet until the restore operation is complete"))
      <> commandGroup "High-level commands"
    )
    <|> apiActionP

waitForSyncP :: Parser (Action m)
waitForSyncP = WaitForSync <$> optional pidP <*> optional outfileP

waitForRestoreP :: Parser (Action m)
waitForRestoreP = WaitForRestore <$> optional pidP <*> optional outfileP

----------------------------------------------------------------------------
-- API action parsers

apiActionP :: Monad m => Parser (Action m)
apiActionP = basic <|> wallet <|> account <|> xact
  where
    basic = commandGroupParser "Basic API calls"
        [ ("node-info", "Query node info", WalletEndpoint <$> nodeInfoP)
        , ("node-settings", "Query static settings of wallet", pure (WalletEndpoint getNodeSettings))
        ]
    wallet = commandGroupParser "Wallets"
        [ ("list-wallets", "List wallets", pure (WalletEndpoint getWallets))
        , ("show-wallet", "Information about a wallet", WalletEndpoint <$> showWalletP)
        , ("edit-wallet", "Update a wallet", WalletEndpoint <$> editWalletP)
        , ("create-wallet", "Create a new wallet from mnemonic", WalletEndpoint <$> createWalletP CreateWallet)
        , ("restore-wallet", "Restore a wallet from mnemonic", WalletEndpoint <$> createWalletP RestoreWallet)
        , ("delete-wallet", "Delete a wallet", WalletEndpointVoid <$> deleteWalletP)
        , ("get-utxo-statistics", "Get UTxO distribution of wallet", WalletEndpoint <$> getUtxoStatisticsP)
        ]
    account = commandGroupParser "Accounts"
        [ ("list-accounts", "Retrieve full list of accounts in wallet", WalletEndpoint <$> listAccountsP)
        , ("show-account", "Information about an account", WalletEndpoint <$> showAccountP)
        , ("account-balance", "Show the balance of an account", WalletEndpoint <$> accountBalanceP)
        , ("create-account", "Add an account to a wallet", WalletEndpoint <$> createAccountP)
        , ("edit-account", "Update an account", WalletEndpoint <$> editAccountP)
        , ("delete-account", "Delete account", WalletEndpointVoid <$> deleteAccountP)
        ]
    xact = commandGroupParser "Addresses and Transactions"
        [ ("list-addresses", "List addresses", WalletEndpoint <$> listAddressesP)
        , ("show-address", "Information about an address", WalletEndpoint <$> showAddressP)
        , ("create-address", "Create an address", WalletEndpoint <$> createAddressP)
        , ("list-transactions", "Get transaction history", WalletEndpoint <$> listTransactionsP)
        , ("send-transaction", "Send transaction to one or multiple target addresses", WalletEndpoint <$> sendTransactionP)
        , ("estimate-fees", "Estimate fees for a transaction", WalletEndpoint <$> estimateFeesP)
        ]
    -- fixme: more endpoints, less boilerplate, user-friendly help text

    commandGroupParser title commands =
        hsubparser (mconcat [ command name (info p (progDesc desc))
                            | (name, desc, p) <- commands]
                    <> commandGroup title <> hidden)

nodeInfoP :: Parser (WalletCall m NodeInfo)
nodeInfoP = flip getNodeInfo <$> ntpCheck
  where
    ntpCheck = flag NoNtpCheck ForceNtpCheck (long "force-ntp-check")

showWalletP :: Parser (WalletCall m Wallet)
showWalletP = flip getWallet <$> walletIdArgP

createWalletP :: WalletOperation -> Parser (WalletCall m Wallet)
createWalletP op = flip postWallet <$> newWalletP
  where
    newWalletP = NewWallet
                 <$> backupPhraseP
                 <*> optional spendingPasswordP
                 <*> assuranceLevelP
                 <*> walletNameP (value "New Wallet")
                 <*> pure op
    backupPhraseP = option parseBackupPhrase (long "backup-phrase" <> metavar "WORDS" <> help "12-word mnemonic")

editWalletP :: Parser (WalletCall m Wallet)
editWalletP = (\wid upd wc -> updateWallet wc wid upd) <$> walletIdArgP <*> updateP
  where
    updateP = WalletUpdate <$> assuranceLevelP <*> walletNameP mempty

deleteWalletP :: Parser (WalletCallVoid m)
deleteWalletP = flip deleteWallet <$> walletIdArgP

deleteAccountP :: Parser (WalletCallVoid m)
deleteAccountP = (\wid accIdx wc -> deleteAccount wc wid accIdx) <$> walletIdArgP <*> accountIndexArgP

listAccountsP :: Monad m => Parser (WalletCall m [Account])
listAccountsP = flip getAccounts <$> walletIdArgP

createAccountP :: Parser (WalletCall m Account)
createAccountP = (\wid acc wc -> postAccount wc wid acc) <$> walletIdArgP <*> newAccountP
  where
    newAccountP = NewAccount <$> optional spendingPasswordP <*> accountNameP (value "New account")

editAccountP :: Parser (WalletCall m Account)
editAccountP = (\wid accIdx upd wc -> updateAccount wc wid accIdx upd)
    <$> walletIdArgP
    <*> accountIndexArgP
    <*> fmap AccountUpdate (accountNameP mempty)

showAccountP :: Parser (WalletCall m Account)
showAccountP = (\wid accIdx wc -> getAccount wc wid accIdx) <$> walletIdArgP <*> accountIndexArgP

accountBalanceP :: Parser (WalletCall m AccountBalance)
accountBalanceP = (\wid accIdx wc -> getAccountBalance wc wid accIdx) <$> walletIdArgP <*> accountIndexArgP

listAddressesP :: Monad m => Parser (WalletCall m [WalletAddress])
listAddressesP = listAccount <|> pure getAddressIndex
  where
    listAccount = (\wid accIdx wc -> getAccountAddressIndex wc wid accIdx)
                  <$> walletIdArgP <*> accountIndexArgP

showAddressP :: Parser (WalletCall m WalletAddress)
showAddressP = flip getAddress <$> addressIdP

createAddressP :: Parser (WalletCall m WalletAddress)
createAddressP = flip postAddress <$> newAddressP
  where
    newAddressP = newAddress
        <$> walletIdArgP
        <*> accountIndexArgP
        <*> optional spendingPasswordP
    -- Change argument order so that optparse help is shown consistently.
    newAddress wid maid sp = NewAddress sp maid wid

sendTransactionP :: Parser (WalletCall m Transaction)
sendTransactionP = flip postTransaction <$> paymentP

estimateFeesP :: Parser (WalletCall m EstimatedFees)
estimateFeesP = flip getTransactionFee <$> paymentP

paymentP :: Parser Payment
paymentP = Payment
           <$> paymentSourceP
           <*> (NonEmpty.fromList <$> some paymentDestinationP)
           <*> optional (WalletInputSelectionPolicy <$> groupingPolicyP)
           <*> optional spendingPasswordP
  where
    paymentSourceP = PaymentSource <$> walletIdArgP <*> accountIndexArgP
    paymentDestinationP = PaymentDistribution <$> walAddressArgP <*> amountP
    groupingPolicyP = flag' Core.OptimizeForHighThroughput (long "group-for-high-throughput" <> help "Do not attempt to group transaction inputs")
                      <|> flag' Core.OptimizeForSecurity (long "group-for-security" <> help "The default strategy to use for selecting transaction inputs")
                      <|> pure Core.OptimizeForSecurity

listTransactionsP :: Monad m => Parser (WalletCall m [Transaction])
listTransactionsP = (\wid acc addr wc -> getTransactionIndex wc wid acc addr)
                    <$> optional walletIdOptP
                    <*> optional accountIndexOptP
                    <*> optional walAddressOptP

getUtxoStatisticsP :: Parser (WalletCall m UtxoStatistics)
getUtxoStatisticsP = flip getUtxoStatistics <$> walletIdArgP

----------------------------------------------------------------------------
-- Little option parsers

pidP :: Parser ProcessID
pidP = option auto (long "pid" <> metavar "PID" <> help "PID of cardano-node")

outfileP :: Parser FilePath
outfileP = strOption (long "out" <> short 'o' <> metavar "FILE" <> help "Output JSON timing info")

parseBackupPhrase :: ReadM BackupPhrase
parseBackupPhrase = eitherReader (first show . fmap BackupPhrase . mkMnemonic . T.words . T.pack)

parsePassPhrase :: ReadM WalletPassPhrase
parsePassPhrase = eitherReader (first show . mkSpendingPassword . T.pack)

walletIdArgP :: Parser WalletId
walletIdArgP = argument walletIdReader (metavar "WALLET"
                                        <> help "Base58-encoded wallet ID hash")

walletIdOptP :: Parser WalletId
walletIdOptP = option walletIdReader (long "wallet-id" <> short 'w'
                                      <> metavar "HASH"
                                      <> help "Base58-encoded Wallet ID")

walletIdReader :: ReadM WalletId
walletIdReader = WalletId . T.pack <$> str

accountIndexArgP :: Parser AccountIndex
accountIndexArgP = argument accountIndexReader (metavar "ACCOUNT" <> help "Account index as integer")

accountIndexOptP :: Parser AccountIndex
accountIndexOptP = option accountIndexReader (long "account" <> short 'n'
                                              <> metavar "INTEGER"
                                              <> help "Account index")

accountIndexReader :: ReadM AccountIndex
accountIndexReader = eitherReader accIndex
  where
    accIndex s = case readMaybe s of
                     Just idx -> first show (mkAccountIndex idx)
                     Nothing  -> Left "Account index is not a number"

addressIdP :: Parser Text
addressIdP = T.pack <$> argument str (metavar "HASH" <> help "Address ID")

walAddressArgP :: Parser WalAddress
walAddressArgP = argument addressReader (metavar "ADDRESS" <> help "Base58-encoded address hash")

walAddressOptP :: Parser WalAddress
walAddressOptP = option addressReader (long "address" <> short 'a'
                                       <> metavar "HASH"
                                       <> help "Base58-encoded address")

addressReader :: ReadM WalAddress
addressReader = WalAddress <$> eitherReader (first T.unpack . Core.decodeTextAddress . T.pack)

spendingPasswordP :: Parser WalletPassPhrase
spendingPasswordP = option parsePassPhrase (long "spending-password" <> metavar "PASSWORD" <> help "32-byte hex-encoded passphrase")

amountP :: Parser WalletCoin
amountP = WalletCoin <$> argument amountReader (metavar "AMOUNT" <> help "Amount in lovelace. Put \"Ada\" after the number to specify the amount in Ada")

amountReader :: ReadM Core.Coin
amountReader = eitherReader (intToCoin <=< parse . T.pack)
  where
    parse = A.parseOnly ((ada <|> lovelace) <* A.endOfInput)
    lovelace = A.decimal <* optional (A.skipSpace *> A.asciiCI "lovelace")
    ada = fmap toLL A.decimal <* (A.skipSpace *> A.asciiCI "ada")
    toLL = (* 1000000)
    intToCoin = first T.unpack . Core.integerToCoin

accountNameP :: Mod OptionFields String -> Parser Text
accountNameP m = T.pack <$> strOption (m <> long "name" <> metavar "NAME" <> help "Name for the account")

walletNameP :: Mod OptionFields String -> Parser Text
walletNameP m = T.pack <$> strOption (m <> long "name" <> metavar "NAME" <> help "Name for the wallet")

assuranceLevelP :: Parser AssuranceLevel --
assuranceLevelP = flag NormalAssurance StrictAssurance
                        (long "strict-assurance"
                         <> help "Assurance level strict")

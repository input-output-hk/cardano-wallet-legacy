{- | The module which contains parsing facilities for
     the CLI options passed to this edge node.
-}
module Cardano.Wallet.Server.CLI where

import           Universum

import           Data.Time.Units (Minute)
import           Data.Version (showVersion)
import           Options.Applicative (Mod, OptionFields, Parser, auto,
                     execParser, footerDoc, fullDesc, header, help, helper,
                     info, infoOption, long, metavar, option, progDesc,
                     showDefault, strOption, switch, value)
import           Paths_cardano_sl (version)
import           Pos.Client.CLI (CommonNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Core.NetworkAddress (NetworkAddress, addrParser, localhost)
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                     compileInfo)
import           Pos.Util.OptParse (fromParsec)
import           Pos.Web (TlsParams (..))


-- | The options parsed from the CLI when starting up this wallet node.
-- This umbrella data type includes the node-specific options for this edge node
-- plus the wallet backend specific options.
data WalletStartupOptions = WalletStartupOptions {
      wsoNodeArgs            :: !CommonNodeArgs
    , wsoWalletBackendParams :: !ChooseWalletBackend
    } deriving Show

-- | TODO: Once we get rid of the legacy wallet, remove this.
data ChooseWalletBackend =
    WalletLegacy !WalletBackendParams
  | WalletNew    !WalletBackendParams
  deriving Show

-- | DB-specific options.
data WalletDBOptions = WalletDBOptions {
      walletDbPath       :: !FilePath
      -- ^ The path for the wallet-backend DB.
    , walletRebuildDb    :: !Bool
      -- ^ Whether or not to wipe and rebuild the DB.
    , walletAcidInterval :: !Minute
      -- ^ The delay between one operation on the acid-state DB and the other.
      -- Such @operation@ entails things like checkpointing the DB.
    , walletFlushDb      :: !Bool
    } deriving Show

-- | The startup parameters for the legacy wallet backend.
-- Named with the suffix `Params` to honour other types of
-- parameters like `NodeParams` or `SscParams`.
data WalletBackendParams = WalletBackendParams
    { enableMonitoringApi     :: !Bool
    -- ^ Whether or not to run the monitoring API.
    --
    -- Note that this parameter is redundant, as we currently delegate to the
    -- Node API and do not run a monitoring server anymore.
    , monitoringApiPort       :: !Word16
    -- ^ The port the monitoring API should listen to.
    --
    -- Note that this parameter is redundant, as we currently delegate to the
    -- Node API and do not run a monitoring server anymore.
    , walletTLSParams         :: !(Maybe TlsParams)
    -- ^ The TLS parameters.
    , walletAddress           :: !NetworkAddress
    -- ^ The wallet address.
    , walletDocAddress        :: !(Maybe NetworkAddress)
    -- ^ The wallet documentation address.
    , walletRunMode           :: !RunMode
    -- ^ The mode this node is running in.
    , walletDbOptions         :: !WalletDBOptions
    -- ^ DB-specific options.
    , forceFullMigration      :: !Bool
    , walletNodeAddress       :: !NetworkAddress
    -- ^ The IP address and port for the node backend.
    , walletNodeTlsClientCert :: FilePath
    -- ^ A filepath to the Node's public certficate
    , walletNodeTlsServerCert :: FilePath
    -- ^ A filepath to the Node's private certficate
    , walletNodeTlsPrivKey    :: FilePath
    -- ^ A filepath to the TLS private key for the Node API.
    , walletNodeTlsPublicKey  :: FilePath
    -- ^ A filepath to the TLS public key for the Node API.
    , walletNodeTlsCaCertPath :: FilePath
    -- ^ A filepath to the TLS CA Certificate for communicating with the Node
    -- API.
    } deriving Show

getWalletDbOptions :: WalletBackendParams -> WalletDBOptions
getWalletDbOptions WalletBackendParams{..} =
    walletDbOptions

getFullMigrationFlag :: WalletBackendParams -> Bool
getFullMigrationFlag WalletBackendParams{..} =
    forceFullMigration

getNodeClientTlsParams :: WalletBackendParams -> TlsParams
getNodeClientTlsParams WalletBackendParams {..} =
    TlsParams
        { tpCertPath = walletNodeTlsClientCert
        , tpCaPath = walletNodeTlsCaCertPath
        , tpKeyPath = walletNodeTlsPrivKey
        , tpClientAuth = False
        }

getNodeServerTlsParams  :: WalletBackendParams -> TlsParams
getNodeServerTlsParams WalletBackendParams {..} =
    TlsParams
        { tpCertPath = walletNodeTlsServerCert
        , tpCaPath = walletNodeTlsCaCertPath
        , tpKeyPath = walletNodeTlsPublicKey
        , tpClientAuth = False
        }

-- | A richer type to specify in which mode we are running this node.
data RunMode = ProductionMode
             -- ^ Run in production mode
             | DebugMode
             -- ^ Run in debug mode
             deriving Show

-- | Converts a @GenesisKeysInclusion@ into a @Bool@.
isDebugMode :: RunMode -> Bool
isDebugMode ProductionMode = False
isDebugMode DebugMode      = True

-- | Parses and returns the @WalletStartupOptions@ from the command line.
getWalletNodeOptions :: HasCompileInfo => IO WalletStartupOptions
getWalletNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> walletStartupOptionsParser) $
        fullDesc <> progDesc "Cardano SL edge node w/ wallet."
                 <> header "Cardano SL edge node."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-wallet-server-" <> showVersion version <>
         ", git revision " <> toString (ctiGitRevision compileInfo))
        (long "version" <> help "Show version.")

-- | The main @Parser@ for the @WalletStartupOptions@
walletStartupOptionsParser :: Parser WalletStartupOptions
walletStartupOptionsParser = WalletStartupOptions <$> CLI.commonNodeArgsParser
                                                  <*> chooseWalletBackendParser

-- | Choose between the two backends
--
-- TODO: This implementation of the parser is not very elegant. I'd prefer
-- something along the lines of
--
-- > chooseWalletBackendParser :: Parser ChooseWalletBackend
-- > chooseWalletBackendParser = asum [
-- >       WalletNew    <$> newWalletBackendParamsParser
-- >     , WalletLegacy <$> walletBackendParamsParser
-- >     ]
--
-- but for some reason this isn't working, perhaps because optparse-applicative
-- isn't backtracking enough? Dunno.
chooseWalletBackendParser :: Parser ChooseWalletBackend
chooseWalletBackendParser = choose
    <$> walletBackendParamsParser
    <*> (switch $ mconcat [
            long "legacy-wallet"
          , help "Use the legacy wallet implementation (NOT RECOMMENDED)"
          ])
  where
    choose opts True  = WalletLegacy $ opts
    choose opts False = WalletNew    $ opts

-- | The @Parser@ for the @WalletBackendParams@.
walletBackendParamsParser :: Parser WalletBackendParams
walletBackendParamsParser = WalletBackendParams <$> enableMonitoringApiParser
                                                <*> monitoringApiPortParser
                                                <*> tlsParamsParser
                                                <*> walletAddressParser
                                                <*> docAddressParser
                                                <*> runModeParser
                                                <*> dbOptionsParser
                                                <*> forceFullMigrationParser
                                                <*> nodeAddressParser
                                                <*> tlsClientCertPathParser
                                                <*> tlsServerCertPathParser
                                                <*> tlsPrivKeyParser
                                                <*> tlsPublicKeyParser
                                                <*> tlsCaCertPathParser
  where
    enableMonitoringApiParser :: Parser Bool
    enableMonitoringApiParser =
        switch
        ( long "monitoring-api"
        <> help
            ( "Activate the node monitoring API. This option doesn't do "
            <> "anything anymore, as the monitoring API has been folded into "
            <> "the API exposed by the node process."
            )
        )

    monitoringApiPortParser :: Parser Word16
    monitoringApiPortParser = CLI.webPortOption 8080
        $ "Port for the monitoring API. This option doesn't do anything "
        <> "anymore, as the monitoring API has been folded into the API exposed"
        <> "by the node process."

    walletAddressParser :: Parser NetworkAddress
    walletAddressParser = networkAddrOption
        $  long "wallet-address"
        <> help "IP and port for backend wallet API."
        <> value (localhost, 8090)

    nodeAddressParser :: Parser NetworkAddress
    nodeAddressParser = networkAddrOption
        $  long "wallet-node-api-address"
        <> help "IP and port for underlying wallet's node monitoring API."
        <> value (localhost, 8080)

    tlsClientCertPathParser :: Parser FilePath
    tlsClientCertPathParser = strOption
        $ long "node-tls-client-cert"
        <> metavar "FILEPATH"
        <> help
            ( "Path to TLS client public certificate used to authenticate to "
            <> "the Node API."
            )

    tlsServerCertPathParser :: Parser FilePath
    tlsServerCertPathParser = strOption
        $ long "node-tls-server-cert"
        <> metavar "FILEPATH"
        <> help
            ( "Path to TLS client public certificate used to authenticate to "
            <> "the Node API."
            )

    tlsPrivKeyParser :: Parser FilePath
    tlsPrivKeyParser = strOption
        $ long "node-tls-key"
        <> metavar "FILEPATH"
        <> help
            ( "Path to TLS client private key used to authenticate to the "
            <> "Node API."
            )

    tlsPublicKeyParser :: Parser FilePath
    tlsPublicKeyParser = strOption
        $ long "node-tls-server-key"
        <> metavar "FILEPATH"
        <> help
            ( "Path to TLS server public key used to authenticate to the "
            <> "Node API."
            )


    tlsCaCertPathParser :: Parser FilePath
    tlsCaCertPathParser = strOption
        $ long "node-tls-ca-cert"
        <> metavar  "FILEPATH"
        <> help
            ( "Path to TLS CA public certificate used to authenticate to the "
            <> "Node API."
            )

    docAddressParser :: Parser (Maybe NetworkAddress)
    docAddressParser = optional $ networkAddrOption
        $  long "wallet-doc-address"
        <> help "IP and port for backend wallet API documentation."

    runModeParser :: Parser RunMode
    runModeParser = (\debugMode -> if debugMode then DebugMode else ProductionMode) <$>
        switch (long "wallet-debug" <>
                help "Run wallet with debug params (e.g. include \
                     \all the genesis keys in the set of secret keys)."
               )

    forceFullMigrationParser :: Parser Bool
    forceFullMigrationParser = switch $
                          long "force-full-wallet-migration" <>
                          help "Enforces a non-lenient migration. \
                               \If something fails (for example a wallet fails to decode from the old format) \
                               \migration will stop and the node will crash, \
                               \instead of just logging the error."

tlsParamsParser :: Parser (Maybe TlsParams)
tlsParamsParser = constructTlsParams <$> certPathParser
                                     <*> keyPathParser
                                     <*> caPathParser
                                     <*> (not <$> noClientAuthParser)
                                     <*> disabledParser
  where
    constructTlsParams tpCertPath tpKeyPath tpCaPath tpClientAuth disabled =
        guard (not disabled) $> TlsParams{..}

    certPathParser :: Parser FilePath
    certPathParser = strOption (CLI.templateParser
                                "tlscert"
                                "FILEPATH"
                                "Path to file with TLS certificate"
                                <> value "scripts/tls-files/server.crt"
                               )

    keyPathParser :: Parser FilePath
    keyPathParser = strOption (CLI.templateParser
                               "tlskey"
                               "FILEPATH"
                               "Path to file with TLS key"
                               <> value "scripts/tls-files/server.key"
                              )

    caPathParser :: Parser FilePath
    caPathParser = strOption (CLI.templateParser
                              "tlsca"
                              "FILEPATH"
                              "Path to file with TLS certificate authority"
                              <> value "scripts/tls-files/ca.crt"
                             )

    noClientAuthParser :: Parser Bool
    noClientAuthParser = switch $
                         long "no-client-auth" <>
                         help "Disable TLS client verification. If turned on, \
                              \no client certificate is required to talk to \
                              \the API."

    disabledParser :: Parser Bool
    disabledParser = switch $
                     long "no-tls" <>
                     help "Disable tls. If set, 'tlscert', 'tlskey' \
                          \and 'tlsca' options are ignored"


-- | The parser for the @WalletDBOptions@.
dbOptionsParser :: Parser WalletDBOptions
dbOptionsParser = WalletDBOptions <$> dbPathParser
                                  <*> rebuildDbParser
                                  <*> acidIntervalParser
                                  <*> flushDbParser
  where
    dbPathParser :: Parser FilePath
    dbPathParser = strOption (long  "wallet-db-path" <>
                              help  "Path to the wallet's database." <>
                              value "wallet-db"
                             )

    rebuildDbParser :: Parser Bool
    rebuildDbParser = switch (long "wallet-rebuild-db" <>
                              help "If wallet's database already exists, discard \
                                   \its contents and create a new one from scratch."
                             )

    acidIntervalParser :: Parser Minute
    acidIntervalParser = fromInteger <$>
        option auto (long "wallet-acid-cleanup-interval" <>
                     help "Interval on which to execute wallet cleanup \
                          \action (create checkpoint and archive and \
                          \cleanup archive partially)" <>
                     metavar "MINUTES" <>
                     value 5
                    )

    flushDbParser :: Parser Bool
    flushDbParser = switch (long "flush-wallet-db" <>
                            help "Flushes all blockchain-recoverable data from DB \
                                 \(everything excluding wallets/accounts/addresses, \
                                 \metadata)"
                           )

-- | Base parser for network addresses. Caller is expected to fill in
-- the flag name, help message and default value if any.
-- e.g.
--
--    networkAddrOption $ long "wallet-address" <> help "API Server Address"
--
networkAddrOption :: Mod OptionFields NetworkAddress -> Parser NetworkAddress
networkAddrOption opts = option (fromParsec addrParser) $ mempty
    <> metavar "IP:PORT"
    <> showDefault
    <> opts

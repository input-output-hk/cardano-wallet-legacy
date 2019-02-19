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
    , wsoWalletBackendParams :: !WalletBackendParams
    } deriving Show

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
    { walletTLSParams      :: !TlsParams
    -- ^ The TLS parameters for the wallet server
    , nodeTLSParams        :: !TlsParams
    -- ^ The TLS parameters for the node server
    , walletAddress        :: !NetworkAddress
    -- ^ The wallet address.
    , walletDocAddress     :: !(Maybe NetworkAddress)
    -- ^ The wallet documentation address.
    , walletDbOptions      :: !WalletDBOptions
    -- ^ DB-specific options.
    , forceFullMigration   :: !Bool
    -- ^ Force non-lenient migration
    , walletNodeAddress    :: !NetworkAddress
    -- ^ The IP address and port for the node backend.
    , walletNodeDocAddress :: !NetworkAddress
    -- ^ The IP address and port for the node backend documentation
    } deriving Show

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
walletStartupOptionsParser = WalletStartupOptions
    <$> CLI.commonNodeArgsParser
    <*> walletBackendParamsParser


-- | The @Parser@ for the @WalletBackendParams@.
walletBackendParamsParser :: Parser WalletBackendParams
walletBackendParamsParser = mkWalletBackendParams
    <$> (not <$> noClientAuthParser)
    <*> caPathParser
    <*> serverCertPathParser
    <*> serverKeyPathParser
    <*> clientCertPathParser
    <*> clientKeyPathParser
    <*> walletAddressParser
    <*> docAddressParser
    <*> dbOptionsParser
    <*> forceFullMigrationParser
    <*> nodeAddressParser
    <*> nodeDocAddressParser
  where
    -- NOTE
    -- We do this to avoid having two flag for the CA cert and the --no-client-auth
    -- (one for the wallet server and one for the node).
    -- This way, we enforce, by construction, that they are the same.
    mkWalletBackendParams auth caCrt srvCrt srvKey clCrt clKey =
        WalletBackendParams
            (TlsParams
                { tpCertPath   = srvCrt
                , tpCaPath     = caCrt
                , tpKeyPath    = srvKey
                , tpClientAuth = auth
                }
            )
            (TlsParams
                { tpCertPath   = clCrt
                , tpCaPath     = caCrt
                , tpKeyPath    = clKey
                , tpClientAuth = auth
                }
            )

    walletAddressParser :: Parser NetworkAddress
    walletAddressParser = networkAddrOption
        $  long "wallet-api-address"
        <> help "IP and port for backend wallet API."
        <> value (localhost, 8090)

    nodeAddressParser :: Parser NetworkAddress
    nodeAddressParser = networkAddrOption
        $  long "node-api-address"
        <> help "IP and port for underlying wallet's node monitoring API."
        <> value (localhost, 8080)

    nodeDocAddressParser :: Parser NetworkAddress
    nodeDocAddressParser = networkAddrOption
        $  long "node-doc-address"
        <> help "IP and port for underlying wallet's node monitoring API Documentation."
        <> value (localhost, 8180)

    docAddressParser :: Parser (Maybe NetworkAddress)
    docAddressParser = optional $ networkAddrOption
        $  long "wallet-doc-address"
        <> help "IP and port for backend wallet API documentation."

    forceFullMigrationParser :: Parser Bool
    forceFullMigrationParser = switch $
                          long "force-full-wallet-migration" <>
                          help "Enforces a non-lenient migration. \
                               \If something fails (for example a wallet fails to decode from the old format) \
                               \migration will stop and the node will crash, \
                               \instead of just logging the error."

    caPathParser :: Parser FilePath
    caPathParser = strOption
        $ long "tls-ca-cert"
        <> metavar "FILEPATH"
        <> help "Path to TLS CA public certificate which signed both wallet and node certificates"

    noClientAuthParser :: Parser Bool
    noClientAuthParser = switch $
        long "tls-no-client-auth" <>
        help "Disable TLS client verification. If turned on,\
            \ no client certificate is required to talk to the API."

    serverCertPathParser :: Parser FilePath
    serverCertPathParser = strOption
        $ long "tls-wallet-server-cert"
        <> metavar "FILEPATH"
        <> help "Path to TLS server public certificate used to identify the wallet server"

    serverKeyPathParser :: Parser FilePath
    serverKeyPathParser = strOption
        $ long "tls-wallet-server-key"
        <> metavar "FILEPATH"
        <> help "Path to TLS server private key used to identify the wallet server"

    clientCertPathParser :: Parser FilePath
    clientCertPathParser = strOption
        $ long "tls-node-client-cert"
        <> metavar "FILEPATH"
        <> help "Path to TLS client public certificate used to authenticate to the node server"

    clientKeyPathParser :: Parser FilePath
    clientKeyPathParser = strOption
        $ long "tls-node-client-key"
        <> metavar "FILEPATH"
        <> help "Path to TLS client private key used to authenticate to the node server"

-- | The parser for the @WalletDBOptions@.
dbOptionsParser :: Parser WalletDBOptions
dbOptionsParser = WalletDBOptions
    <$> dbPathParser
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

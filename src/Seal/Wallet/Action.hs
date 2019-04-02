{-# LANGUAGE LambdaCase #-}
module Seal.Wallet.Action (actionWithWallet) where

import           Universum

-- import qualified Data.ByteString.Char8 as BS8
import           Ntp.Client (NtpConfiguration, ntpClientSettings, withNtpClient)

import           Seal.Chain.Genesis as Genesis (Config (..))
import           Seal.Chain.Ssc (SscParams)
import           Seal.Chain.Txp (TxpConfiguration)
import           Seal.Context (ncUserSecret)
import           Seal.Launcher (NodeParams (..), NodeResources (..),
                    bpLoggingParams, lpDefaultName, runNode)
import           Seal.Launcher.Configuration (WalletConfiguration (..), HasConfigurations)
import           Seal.Util.CompileInfo (HasCompileInfo)
import           Seal.Util.Wlog (LoggerName, Severity (..), logInfo, logMessage,
                        usingLoggerName)
import           Seal.WorkMode (EmptyMempoolExt)

-- import           Seal.Wallet.API.Node.Client (NodeHttpClient)
import qualified Seal.Wallet.API.V1.Headers as Headers
import           Seal.Wallet.Kernel (PassiveWallet)
import qualified Seal.Wallet.Kernel as Kernel
import qualified Seal.Wallet.Kernel.Internal as Kernel.Internal
import qualified Seal.Wallet.Kernel.Keystore as Keystore
import           Seal.Wallet.Kernel.Migration (migrateLegacyDataLayer)
import qualified Seal.Wallet.Kernel.Mode as Kernel.Mode
import           Seal.Wallet.Kernel.NodeStateAdaptor (newNodeStateAdaptor)
import           Seal.Wallet.Server.CLI (WalletBackendParams (..),
                        walletDbPath, walletNodeAddress, walletRebuildDb)
import           Seal.Wallet.Server.Middlewares
                        (faultInjectionHandleIgnoreAPI, throttleMiddleware,
                        unsupportedMimeTypeMiddleware, withDefaultHeader)
import qualified Seal.Wallet.Server.Plugins as Plugins
import           Seal.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Seal.Wallet.WalletLayer.Kernel as WalletLayer.Kernel


-- | The "workhorse" responsible for starting a Seal edge node plus a number of extra plugins.
actionWithWallet
    :: (HasConfigurations, HasCompileInfo)
    => WalletBackendParams
    -> Genesis.Config
    -> WalletConfiguration
    -> TxpConfiguration
    -> NtpConfiguration
    -> NodeParams
    -> SscParams
    -> NodeResources EmptyMempoolExt
    -> IO ()
actionWithWallet
    params@(WalletBackendParams{..})
    genesisConfig walletConfig txpConfig ntpConfig nodeParams _ nodeRes = do
    logInfo "[Attention] Software is built with the wallet backend"
    ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
    userSecret <- readTVarIO (ncUserSecret $ nrContext nodeRes)

    let nodeState = newNodeStateAdaptor
            genesisConfig
            nodeRes
            ntpStatus

    liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore -> do
        let dbPath = walletDbPath walletDbOptions
        let rebuildDB = walletRebuildDb walletDbOptions
        let dbMode = Kernel.UseFilePath (Kernel.DatabaseOptions {
              Kernel.dbPathAcidState = dbPath <> "-acid"
            , Kernel.dbPathMetadata  = dbPath <> "-sqlite.sqlite3"
            , Kernel.dbRebuild       = rebuildDB
            })
        let pm = configProtocolMagic genesisConfig
        WalletLayer.Kernel.bracketPassiveWallet pm dbMode logMessage' keystore nodeState (npFInjects nodeParams) $ \walletLayer passiveWallet -> do
            migrateLegacyDataLayer passiveWallet dbPath forceFullMigration

            let plugs = plugins (walletLayer, passiveWallet) dbMode

            Kernel.Mode.runWalletMode
                genesisConfig
                txpConfig
                nodeRes
                walletLayer
                (runNode genesisConfig txpConfig nodeRes plugs)
  where

    plugins :: (PassiveWalletLayer IO, PassiveWallet)
            -> Kernel.DatabaseMode
            -> [ (Text, Plugins.Plugin Kernel.Mode.WalletMode) ]
    plugins w dbMode = concat [
            -- The actual wallet backend server.
            [
              ("wallet-new api worker", Plugins.apiServer params genesisConfig w
                [ faultInjectionHandleIgnoreAPI (npFInjects nodeParams) -- This allows dynamic control of fault injection
                , throttleMiddleware (ccThrottle walletConfig)          -- Throttle requests
                , withDefaultHeader Headers.applicationJson
                , unsupportedMimeTypeMiddleware
                ])

            -- Periodically compact & snapshot the acid-state database.
            , ("acid state cleanup", Plugins.acidStateSnapshots (view Kernel.Internal.wallets (snd w)) params dbMode)
            -- , ("node monitoring server", Plugins.nodeAPIServer params genesisConfig ntpConfig nodeRes)
            ]
        -- The corresponding wallet documention, served as a different
        -- server which doesn't require client x509 certificates to
        -- connect, but still serves the doc through TLS
        , maybe [] (pure . ("doc server",)) (Plugins.docServer params)
        ]

    -- Extract the logger name from node parameters
    --
    -- TODO: Not sure what the policy is for logger names of components.
    -- For now we just use the one from the node itself.
    logMessage' :: Severity -> Text -> IO ()
    logMessage' sev txt =
        usingLoggerName loggerName $ logMessage sev txt
      where
        loggerName :: LoggerName
        loggerName = lpDefaultName . bpLoggingParams . npBaseParams $ nodeParams

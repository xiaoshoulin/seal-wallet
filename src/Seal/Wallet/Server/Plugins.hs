{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}

-- Orphan instance for Buildable Servant.NoContent
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Seal.Wallet.Server.Plugins
    ( Plugin
    , apiServer
    , docServer
    , acidStateSnapshots
    , setupNodeClient
    ) where

import           Universum

import           Data.Acid (AcidState)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import           Data.Typeable (typeOf)
import           Formatting.Buildable (build)
import           Network.HTTP.Types.Status (badRequest400)
import           Network.Wai (Application, Middleware, Response, responseLBS)
import           Network.Wai.Handler.Warp (setOnException, setOnExceptionResponse)
import qualified Network.Wai.Handler.Warp as Warp
import           Seal.Chain.Genesis
-- import           Seal.Chain.Update (updateConfiguration)
-- import           Seal.Client.CLI (NodeApiArgs (..))
import qualified Servant
import           Servant.Client (Scheme (..))
import qualified Servant.Client as Servant

-- import           Seal.Wallet.Client.CLI (launchNodeServer)
-- import           Seal.Node.Client (NodeHttpClient)
-- import qualified Seal.Node.Client as NodeClient
import qualified Seal.Node.Manager as NodeManager
import           Seal.NodeIPC (startNodeJsIPC)
import           Seal.Wallet.API as API
import           Seal.Wallet.API.V1.Headers (applicationJson)
import           Seal.Wallet.API.V1.ReifyWalletError
                     (translateWalletLayerErrors)
import           Seal.Wallet.API.Node.Client (NodeHttpClient)
import qualified Seal.Wallet.API.V1.Types as V1
import           Seal.Wallet.Kernel (DatabaseMode (..), PassiveWallet)
import qualified Seal.Wallet.Kernel.Diffusion as Kernel
import qualified Seal.Wallet.Kernel.Mode as Kernel
-- import qualified Seal.Wallet.Kernel.NodeStateAdaptor as NodeStateAdaptor
import           Seal.Wallet.Node.Client (mkHttpClient)
import qualified Seal.Wallet.Server as Server
import           Seal.Wallet.Server.CLI (WalletBackendParams (..),
                     WalletBackendParams (..), walletAcidInterval,
                     walletDbOptions)
import           Seal.Wallet.Server.Middlewares (withMiddlewares)
import           Seal.Wallet.Server.Plugins.AcidState
                     (createAndArchiveCheckpoints)
import           Seal.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import qualified Seal.Wallet.WalletLayer.Kernel as WalletLayer.Kernel
-- import           Ntp.Client (NtpConfiguration)
-- import           Seal.Launcher.Resource (NodeResources (..))
import           Seal.Web.Server (serveDocImpl)
import           Seal.Infra.Diffusion.Types (Diffusion (..))
import           Seal.Infra.Shutdown (HasShutdownContext (shutdownContext),
                     ShutdownContext)
import           Seal.Launcher.Configuration (HasConfigurations)
import           Seal.Util.CompileInfo (HasCompileInfo)
import           Seal.Util.Wlog (logInfo, modifyLoggerName, usingLoggerName)
import           Seal.Web (TlsParams (..), serveImpl)


-- A @Plugin@ running in the monad @m@.
type Plugin m = Diffusion m -> m ()

-- | Override defautl Warp settings to avoid printing exception to console.
-- They're already printing to logfile!
defaultSettings :: Warp.Settings
defaultSettings = Warp.defaultSettings
    & setOnException (\_ _ -> return ())

-- | A @Plugin@ to start the wallet REST server
apiServer
    :: WalletBackendParams
    -> Config
    -> (PassiveWalletLayer IO, PassiveWallet)
    -> [Middleware]
    -> Plugin Kernel.WalletMode
apiServer
    WalletBackendParams{..}
    config
    (passiveLayer, passiveWallet)
    middlewares
    diffusion
  = do
    env <- ask
    let diffusion' = Kernel.fromDiffusion (lower env) diffusion
    logInfo "Testing node client connection"
    WalletLayer.Kernel.bracketActiveWallet passiveLayer passiveWallet diffusion' $ \active _ -> do
        ctx <- view shutdownContext
        serveImpl
            (getApplication active config)
            (BS8.unpack ip)
            port
            walletTLSParams
            (Just $ setOnExceptionResponse exceptionHandler defaultSettings)
            (Just $ portCallback ctx)
  where
    (ip, port) = walletAddress

    exceptionHandler :: SomeException -> Response
    exceptionHandler se = case translateWalletLayerErrors se of
            Just we -> handleLayerError we
            Nothing -> handleGenericError se

    -- Handle domain-specific errors coming from the Wallet Layer
    handleLayerError :: V1.WalletError -> Response
    handleLayerError we =
            responseLBS (V1.toHttpErrorStatus we) [applicationJson] . encode $ we

    -- Handle general exceptions
    handleGenericError :: SomeException -> Response
    handleGenericError (SomeException se) =
        responseLBS badRequest400 [applicationJson] $ encode defWalletError
        where
            -- NOTE: to ensure that we don't leak any sensitive information,
            --       we only reveal the exception type here.
            defWalletError = V1.UnknownError $ T.pack . show $ typeOf se

    getApplication
        :: ActiveWalletLayer IO
        -> Config
        -> Kernel.WalletMode Application
    getApplication active gconfig = do
        logInfo "New wallet API has STARTED!"
        return
            $ withMiddlewares middlewares
            $ Servant.serve API.walletAPI
            $ Server.walletServer active gconfig

    lower :: env -> ReaderT env IO a -> IO a
    lower env m = runReaderT m env

    portCallback :: ShutdownContext -> Word16 -> IO ()
    portCallback ctx =
        usingLoggerName "NodeIPC" . flip runReaderT ctx . startNodeJsIPC

-- | A @Plugin@ to serve the wallet documentation
docServer
    :: (HasConfigurations, HasCompileInfo)
    => WalletBackendParams
    -> Maybe (Plugin Kernel.WalletMode)
docServer WalletBackendParams{walletDocAddress = Nothing} = Nothing
docServer WalletBackendParams{walletDocAddress = Just (ip, port), walletTLSParams} = Just (const $ makeWalletServer)
  where
    makeWalletServer = serveDocImpl
        application
        (BS8.unpack ip)
        port
        walletTLSParams
        (Just defaultSettings)
        Nothing

    application :: Kernel.WalletMode Application
    application =
        return $ Servant.serve API.walletDoc Server.walletDocServer

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidStateSnapshots :: AcidState db
                   -> WalletBackendParams
                   -> DatabaseMode
                   -> Plugin Kernel.WalletMode
acidStateSnapshots dbRef params dbMode = const worker
  where
    worker = do
      let opts = walletDbOptions params
      modifyLoggerName (const "acid-state-checkpoint-plugin") $
          createAndArchiveCheckpoints
              dbRef
              (walletAcidInterval opts)
              dbMode

instance Buildable Servant.NoContent where
    build Servant.NoContent = build ()

setupNodeClient
    :: MonadIO m
    => (String, Int)
    -> TlsParams
    -> m NodeHttpClient
setupNodeClient (serverHost, serverPort) params = liftIO $ do
    let serverId = (serverHost, BS8.pack $ show serverPort)
    caChain <- NodeManager.readSignedObject (tpCaPath params)
    clientCredentials <- NodeManager.credentialLoadX509 (tpCertPath params) (tpKeyPath params) >>= \case
        Right   a -> return a
        Left  err -> fail $ "Error decoding X509 certificates: " <> err
    manager <- NodeManager.newManager $ NodeManager.mkHttpsManagerSettings serverId caChain clientCredentials

    let
        baseUrl = Servant.BaseUrl Https serverHost serverPort mempty
        walletClient :: NodeHttpClient
        walletClient = mkHttpClient baseUrl manager

    return walletClient

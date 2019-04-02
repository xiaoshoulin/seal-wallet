module Seal.Wallet.API.V1.Handlers.Settings (handlers, getNodeSettings) where

import           Universum

import           Servant

import qualified Seal.Wallet.API.Node.Client as NodeClient
import           Seal.Wallet.API.Response (APIResponse, single)
import           Seal.Wallet.API.V1.Types (NodeSettings)
import           Seal.Wallet.NodeProxy (NodeHttpClient, handleNodeError)
import qualified Seal.Wallet.API.V1.Info as Node

handlers
    :: NodeHttpClient
    -> ServerT Node.SettingsAPI Handler
handlers = getNodeSettings

-- | Retrieve the static settings for this node
getNodeSettings
    :: NodeHttpClient
    -> Handler (APIResponse NodeSettings)
getNodeSettings nc = do
    emUpd <- liftIO . runExceptT $ NodeClient.getNodeSettings nc
    case emUpd of
        Left err  ->
            handleNodeError err
        Right settings ->
            single <$> pure settings

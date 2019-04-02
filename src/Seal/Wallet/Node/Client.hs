{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Seal.Wallet.Node.Client
    ( -- Node Client
      NodeV1Api
    , nodeV1Api
    , ClientError(..)
    , fromServantError

    -- * HTTP instance
    , NodeHttpClient
    , mkHttpClient
    -- * Deprecated
    , applyUpdate
    , postponeUpdate
    ) where

import           Universum

import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Types (status404)
import           Servant ((:<|>) (..), (:>))
import           Servant.Client (BaseUrl (..), ClientEnv (..), ClientM,
                     GenResponse (..), ServantError, client, runClientM)
import qualified Servant.Client as Servant

import           Seal.Wallet.API.V1.Info (API, SettingsAPI)
import           Seal.Wallet.API.Node.Client (NodeClient (..), NodeHttpClient,
                     ClientError (..))
import           Seal.Wallet.API.Response.JSend (ResponseStatus (..))
import           Seal.Wallet.API.Util.Servant (APIResponse (..))
import qualified Seal.Web as Legacy


type NodeV1Api
    = "api" :> "v1" :>
    (       API
    :<|>    Legacy.NodeApi
    )

nodeV1Api :: Proxy NodeV1Api
nodeV1Api = Proxy
-- * Node Client

settingsAPI :: Proxy SettingsAPI
settingsAPI = Proxy


-- | A backwards compatibility wrapper for 'restartNode'.
applyUpdate :: NodeClient m -> m ()
applyUpdate = restartNode
{-# DEPRECATED applyUpdate "Use 'restartNode' instead." #-}

-- | 'postponeUpdate' was removed from the API. This is a backwards
-- compatibility wrapper that is deprecated.
postponeUpdate :: Applicative m => NodeClient n -> m ()
postponeUpdate _ = pure ()
{-# DEPRECATED postponeUpdate "This endpoint was turned into a noop." #-}

fromServantError :: FromJSON a => ServantError -> ClientError a
fromServantError err = case err of
    Servant.FailureResponse r@(Response s _ _ body)
        | s == status404 ->
            ErrFromServant err
        | otherwise ->
            case Aeson.decode body of
                Just (APIResponse a ErrorStatus _) ->
                    KnownError a
                Just _ ->
                    ErrFromServant $ Servant.DecodeFailure "API failed with non-error response ?!?" r
                Nothing ->
                    ErrFromServant $ Servant.DecodeFailure "Invalid / Non-JSEnd API Error Response" r
    _ ->
        ErrFromServant err

-- * HTTP Instance

mkHttpClient
    :: BaseUrl
    -> Manager
    -> NodeHttpClient
mkHttpClient baseUrl manager = NodeClient
    { getUtxo =
        run getUtxoR
    , getConfirmedProposals =
        run getConfirmedProposalsR
    , getNodeSettings =
        fmap wrData $ run getNodeSettingsR
    -- , getNodeInfo =
    --     fmap wrData . run . getNodeInfoR
    , getNextUpdate =
        wrData <$> run getNextUpdateR
    , restartNode =
        void $ run restartNodeR
    }
  where
    run :: forall a. ClientM a -> ExceptT (ClientError ()) IO a
    run = ExceptT
        . fmap (first fromServantError)
        . flip runClientM (ClientEnv manager baseUrl noCookieJar)

    noCookieJar = Nothing

    getNodeSettingsR = client settingsAPI

    (       
        -- getNodeSettingsR
    --  :<|>   getNodeInfoR
    --  :<|>   
            getNextUpdateR
     :<|>   restartNodeR
     ):<|>( _
     :<|>   getUtxoR
     :<|>   _
     :<|>   _
     :<|>   _
     :<|>   getConfirmedProposalsR
     :<|>   (_)
     ) = client nodeV1Api


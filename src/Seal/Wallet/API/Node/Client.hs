{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Seal.Wallet.API.Node.Client
    ( -- Node Client
      NodeClient(..)
    , ClientError(..)

    -- * HTTP instance
    , NodeHttpClient
    -- * Deprecated
    , applyUpdate
    , postponeUpdate
    ) where

import           Universum

import qualified Servant.Client as Servant

import           Seal.Chain.Txp (TxOut)
import qualified Seal.Chain.Update as Core
import           Seal.Wallet.API.V1.Types (NodeSettings, V1)
import           Seal.Web.Types (CConfirmedProposalState)


-- * Node Client

data NodeClient m
    = NodeClient
    { getUtxo
        :: m [TxOut]

    , getConfirmedProposals
        :: m [CConfirmedProposalState]

    , getNodeSettings
        :: m NodeSettings

    -- , getNodeInfo
    --     :: ForceNtpCheck
    --     -> m NodeInfo

    , restartNode
        :: m ()

    , getNextUpdate
        :: m (V1 Core.SoftwareVersion)
    } deriving (Generic)


-- | A backwards compatibility wrapper for 'restartNode'.
applyUpdate :: NodeClient m -> m ()
applyUpdate = restartNode
{-# DEPRECATED applyUpdate "Use 'restartNode' instead." #-}

-- | 'postponeUpdate' was removed from the API. This is a backwards
-- compatibility wrapper that is deprecated.
postponeUpdate :: Applicative m => NodeClient n -> m ()
postponeUpdate _ = pure ()
{-# DEPRECATED postponeUpdate "This endpoint was turned into a noop." #-}

data ClientError a
    = KnownError a
    | ErrFromServant Servant.ServantError
    deriving (Show, Generic, Eq)

type NodeHttpClient = NodeClient (ExceptT (ClientError ()) IO)
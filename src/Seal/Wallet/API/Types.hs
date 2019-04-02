{-- Types shared between different API versions. --}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RankNTypes      #-}

module Seal.Wallet.API.Types
    ( DQueryParam
    , DHeader
    , mapRouter
    , WithDefaultApiArg
    , Tag
    , TagDescription (..)
    , WalletLoggingConfig
    , CustomQueryFlag 
    ) where

import           Universum

import qualified Data.Aeson.Options as Aeson
import           Data.Aeson.TH (deriveJSON)
import           Data.Reflection (Reifies (..))
import           GHC.TypeLits (Symbol)
import           Servant
-- import           Servant.Client (Client, HasClient (..))
import qualified Servant.Server.Internal as SI

import           Seal.Util.Servant (ApiLoggingConfig (..), DQueryParam, WithDefaultApiArg)
import           Seal.Wallet.API.Util.Pagination
import           Seal.Wallet.API.Util.Servant (DHeader, Tag, TagDescription (..))

-- | `mapRouter` is helper function used in order to transform one `HasServer`
-- instance to another. It can be used to introduce custom request params type.
-- See e. g. `WithDefaultApiArg` as an example of usage
mapRouter
    :: forall api api' ctx env.
       (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
    -> (Server api' -> Server api)
    -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
mapRouter routing f = \_ ctx delayed -> routing Proxy ctx (fmap f delayed)

-- | Extra information associated with an HTTP response.
data Metadata = Metadata
  { metaPagination   :: PaginationMetadata
    -- ^ Pagination-specific metadata
  } deriving (Show, Eq)

deriveJSON Aeson.defaultOptions ''Metadata

-- | Specifes logging config for servant.
data WalletLoggingConfig

-- If logger config will ever be determined in runtime, 'Data.Reflection.reify'
-- can be used.
-- | Raises 'WalletLoggingConfig' at type level.
instance Reifies WalletLoggingConfig ApiLoggingConfig where
    reflect _ = ApiLoggingConfig ("wallet" <> "api")

data CustomQueryFlag (sym :: Symbol) flag 

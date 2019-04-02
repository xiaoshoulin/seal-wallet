module Seal.Wallet.API
       ( -- * Wallet API Top-Level Representations
         WalletAPI
       , walletAPI
       , WalletDoc
       , walletDoc
       , walletDocAPI

         -- * Components Representations
       , V1API
       , v1API
       , InternalAPI
       , internalAPI
       ) where

import           Seal.Wallet.API.Types (WalletLoggingConfig)
import           Seal.Util.Servant (LoggingApi)
import           Servant ((:>), Proxy (..))
import           Servant.Swagger.UI (SwaggerSchemaUI)

import qualified Seal.Wallet.API.Internal as Internal
import qualified Seal.Wallet.API.V1 as V1

-- | The complete API, qualified by its versions. For backward compatibility's
-- sake, we still expose the old API under @/api/@. Specification is split under
-- separate modules.
--
-- Unsurprisingly:
--
-- * 'Seal.Wallet.API.V1' hosts the full specification of the V1 API;
--
-- This project uses Servant, which means the logic is separated from the
-- implementation (i.e. the Server). Such server, together with all its web
-- handlers lives in an executable which contains the aptly-named modules:
--
-- * 'Seal.Wallet.Server' contains the main server;
-- * 'Seal.Wallet.API.V1.Handlers' contains all the @Handler@s serving the V1 API;
-- * 'Seal.Wallet.API.Internal.Handlers' contains all the @Handler@s serving the Internal API;

type WalletAPI = LoggingApi WalletLoggingConfig V1API
walletAPI :: Proxy WalletAPI
walletAPI = Proxy

type WalletDoc = "docs" :> "v1" :> SwaggerSchemaUI "index" "swagger.json"
walletDoc :: Proxy WalletDoc
walletDoc = Proxy
walletDocAPI :: Proxy V1API
walletDocAPI = Proxy


type V1API = "api" :> "v1" :> V1.API
v1API :: Proxy V1API
v1API = Proxy

type InternalAPI = "api" :> "internal" :> Internal.API
internalAPI :: Proxy InternalAPI
internalAPI = Proxy

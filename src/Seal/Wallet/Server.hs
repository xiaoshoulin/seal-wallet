module Seal.Wallet.Server
    ( walletServer
    , walletDocServer
    , curSoftwareVersion
    ) where

import           Universum

import           Servant
import           Seal.Chain.Genesis
-- import           Seal.Wallet.API.Node.Client (NodeHttpClient)
import           Seal.Chain.Update (UpdateConfiguration (..), HasUpdateConfiguration, 
                     SoftwareVersion (..), updateConfiguration)
import           Seal.Util.CompileInfo (HasCompileInfo, compileInfo)

import           Seal.Wallet.API
-- import qualified Seal.Wallet.API.Internal.Handlers as Internal
import qualified Seal.Wallet.API.V1.Handlers as V1
import           Seal.Wallet.API.V1.Swagger (swaggerSchemaUIServer)
import qualified Seal.Wallet.API.V1.Swagger as Swagger
import           Seal.Wallet.WalletLayer (ActiveWalletLayer (..))

-- | Serve the REST interface to the wallet
--
-- NOTE: Unlike the legacy server, the handlers will not run in a special
-- Seal monad because they just interfact with the Wallet object.
walletServer
    :: ActiveWalletLayer IO
    -> Config
    -> Server WalletAPI
walletServer w config =
    v1Handler
    -- :<|> internalHandler
  where
    v1Handler       = V1.handlers w config
    -- internalHandler = Internal.handlers nc (walletPassiveLayer w)

walletDocServer :: (HasCompileInfo, HasUpdateConfiguration) => Server WalletDoc
walletDocServer =
    v1DocHandler
  where
    infos        = (compileInfo, curSoftwareVersion updateConfiguration)
    v1DocHandler = swaggerSchemaUIServer
        (Swagger.api infos walletDocAPI Swagger.highLevelDescription)

curSoftwareVersion :: UpdateConfiguration -> SoftwareVersion
curSoftwareVersion uc = SoftwareVersion (ccApplicationName uc) (ccApplicationVersion uc)

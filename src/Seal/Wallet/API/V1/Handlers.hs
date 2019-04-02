module Seal.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import           Seal.Chain.Genesis
import qualified Seal.Wallet.API.V1 as V1
import qualified Seal.Wallet.API.V1.Handlers.Accounts as Accounts
import qualified Seal.Wallet.API.V1.Handlers.Addresses as Addresses
-- import qualified Seal.Wallet.API.V1.Handlers.Info as Info
-- import qualified Seal.Wallet.API.V1.Handlers.Settings as Settings
import qualified Seal.Wallet.API.V1.Handlers.Transactions as Transactions
import qualified Seal.Wallet.API.V1.Handlers.Wallets as Wallets

-- import           Seal.Wallet.NodeProxy (NodeHttpClient)
import           Seal.Wallet.WalletLayer (ActiveWalletLayer,
                     walletPassiveLayer)


handlers :: ActiveWalletLayer IO 
         -> Config
         -> Server V1.API
handlers aw config =
    Addresses.handlers pw
    :<|> Wallets.fullyOwnedHandlers pw
    :<|> Wallets.externallyOwnedHandlers pw
    :<|> Accounts.handlers pw
    :<|> Transactions.handlers aw config
    -- :<|> Settings.handlers nc
    -- :<|> Info.handlers nc
  where
    pw = walletPassiveLayer aw

module Seal.Wallet.API.V1 where


import           Servant ((:<|>))

import qualified Seal.Wallet.API.V1.Accounts as Accounts
import qualified Seal.Wallet.API.V1.Addresses as Addresses
import qualified Seal.Wallet.API.V1.Transactions as Transactions
import qualified Seal.Wallet.API.V1.Wallets as Wallets

type API =  Addresses.API
       :<|> Wallets.FullyOwnedAPI
       :<|> Wallets.ExternallyOwnedAPI
       :<|> Accounts.API
       :<|> Transactions.API

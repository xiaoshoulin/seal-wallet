module Seal.Wallet.API.V1.Accounts where

import           Servant

import           Seal.Wallet.API.Request
import           Seal.Wallet.API.Response
import           Seal.Wallet.API.V1.Parameters
import           Seal.Wallet.API.V1.Types


type API
    = 
         "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Deletes an Account."
          :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Retrieves a specific Account."
          :> Get '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> WalletRequestParams
          :> Summary "Retrieves the full list of Accounts."
          :> Get '[ValidJSON] (APIResponse [Account])
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> Summary "Creates a new Account for the given Wallet."
          :> ReqBody '[ValidJSON] (New Account)
          :> Post '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId
          :> Summary "Update an Account for the given Wallet."
          :> ReqBody '[ValidJSON] (Update Account)
          :> Put '[ValidJSON] (APIResponse Account)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId :> "addresses"
          :> Summary "Retrieve only account's addresses."
          :> WalletRequestParams
          :> FilterBy '[WalAddress] WalletAddress
          :> Get '[ValidJSON] (APIResponse AccountAddresses)
    :<|> "wallets" :> CaptureWalletId :> "accounts"
          :> CaptureAccountId :> "amount"
          :> Summary "Retrieve only account's balance."
          :> Get '[ValidJSON] (APIResponse AccountBalance)

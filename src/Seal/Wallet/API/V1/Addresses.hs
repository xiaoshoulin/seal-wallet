module Seal.Wallet.API.V1.Addresses where

import           Servant
import           Universum (Text)

import           Seal.Wallet.API.Response
import           Seal.Wallet.API.V1.Parameters
import           Seal.Wallet.API.V1.Types


type API = 
           "addresses" :> WalletRequestParams
                       :> Summary "Returns a list of the addresses."
                       :> Get '[ValidJSON] (APIResponse [WalletAddress])
      :<|> "addresses" :> ReqBody '[ValidJSON] NewAddress
                       :> Summary "Creates a new Address."
                       :> Post '[ValidJSON] (APIResponse WalletAddress)
      :<|> "addresses" :> Capture "address" Text
                       :> Summary "Returns interesting information about an address, if available and valid."
                       :> Get '[ValidJSON] (APIResponse WalletAddress)
      :<|> "wallets" :> CaptureWalletId :> "addresses"
        :> Summary "Batch import existing addresses"
        :> ReqBody '[ValidJSON] [WalAddress]
        :> Post '[ValidJSON] (APIResponse (BatchImportResult WalAddress))

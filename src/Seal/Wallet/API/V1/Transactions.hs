module Seal.Wallet.API.V1.Transactions where

import           Seal.Wallet.API.Request
import           Seal.Wallet.API.Response
import           Seal.Wallet.API.V1.Parameters
import           Seal.Wallet.API.V1.Types

import           Servant

type API = 
         "transactions" :> Summary "Generates a new transaction from the source to one or multiple target addresses."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (APIResponse Transaction)
    :<|> "transactions" :> Summary "Returns the transaction history, i.e the list of all the past transactions."
                        :> QueryParam "wallet_id" WalletId
                        :> QueryParam "account_index" AccountIndex
                        :> QueryParam "address" WalAddress
                        :> WalletRequestParams
                        :> FilterBy '[ WalletTxId
                                     , WalletTimestamp
                                     ] Transaction
                        :> SortBy   '[ WalletTimestamp
                                     ] Transaction
                        :> Get '[ValidJSON] (APIResponse [Transaction])
    :<|> "transactions" :> "fees"
                        :> Summary "Estimate the fees which would originate from the payment."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (APIResponse EstimatedFees)
    :<|> "transactions" :> "unsigned"
                        :> Summary "Creates a new unsigned transaction."
                        :> ReqBody '[ValidJSON] Payment
                        :> Post '[ValidJSON] (APIResponse UnsignedTransaction)

    :<|> "transactions" :> "externally-signed"
                        :> Summary "Publish an externally-signed transaction."
                        :> ReqBody '[ValidJSON] SignedTransaction
                        :> Post '[ValidJSON] (APIResponse Transaction)

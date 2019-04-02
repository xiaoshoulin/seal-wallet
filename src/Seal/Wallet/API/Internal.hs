-- | This module contains the top level API definition for frontend-related
-- tasks.  The API endpoints presented here are intended for use with the
-- Daedalus client, and aren't useful for wallets, exchanges, and other users.
module Seal.Wallet.API.Internal where

import           Servant

import           Seal.Wallet.API.Response (APIResponse, ValidJSON)
import           Seal.Wallet.API.V1.Types (Wallet, WalletImport,
                     WalletSoftwareVersion)

type API =
           "next-update"
        :> Summary "Version of the next update (404 if none)"
        :> Get '[ValidJSON] (APIResponse WalletSoftwareVersion)

    :<|> "apply-update"
        :> Summary "Apply the next available update"
        :> Post '[ValidJSON] NoContent

    :<|> "postpone-update"
        :> Summary "Discard and postpone the next available update"
        :> Post '[ValidJSON] NoContent

    :<|> "reset-wallet-state"
        :> Summary "Clear wallet state and all associated secret keys"
        :> DeleteNoContent '[ValidJSON] NoContent
    :<|> "import-wallet"
        :> Summary "Import a Wallet from disk."
        :> ReqBody '[ValidJSON] WalletImport
        :> Post '[ValidJSON] (APIResponse Wallet)

module Seal.Wallet.API.V1.Settings where

import           Seal.Wallet.API.Response (APIResponse, ValidJSON)
import           Seal.Wallet.API.V1.Types

import           Servant

type API = "node-settings"  :> Summary "Retrieves the static settings for this node."
                            :> Get '[ValidJSON] (APIResponse NodeSettings)
         

module Main where

import           Universum

import           Seal.Client.CLI (NodeArgs (..), loggingParams)
import           Seal.Launcher (launchNode)
import           Seal.Util.CompileInfo (withCompileInfo)

import           Seal.Wallet.Action (actionWithWallet)
import           Seal.Wallet.Server.CLI (WalletStartupOptions (..),
                     getWalletNodeOptions)


-- | The main entrypoint for the Wallet.
main :: IO ()
main = 
    withCompileInfo $ do
    WalletStartupOptions cArgs wArgs <- getWalletNodeOptions
    let lArgs = loggingParams "node" cArgs
    let nArgs = NodeArgs { behaviorConfigPath = Nothing }

    putText "Wallet is starting..."

    launchNode nArgs cArgs lArgs (actionWithWallet wArgs)

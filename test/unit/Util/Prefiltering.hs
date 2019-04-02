module Util.Prefiltering
    ( prefilterUtxo
    ) where

import           Universum

import qualified Data.Map.Strict as Map

import           Seal.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Seal.Wallet.Kernel.DB.HdWallet (HdAccountId, HdAddress,
                     hdAddressId, hdAddressIdParent, isOurs)
import           Seal.Chain.Txp (TxOut (..), TxOutAux (..), Utxo)
import           Seal.Crypto (EncryptedSecretKey)

prefilterUtxo
    :: HdRootId
    -> EncryptedSecretKey
    -> Utxo
    -> Map HdAccountId (Utxo, [HdAddress])
prefilterUtxo rootId esk utxo = flip evalState (Map.singleton rootId esk) $
    fmap (Map.unionsWith (<>)) $ forM (Map.toList utxo) $ \(txin, txout) -> do
        let addr = txOutAddress $ toaOut txout
        state (isOurs addr) <&> \case
            Nothing ->
                Map.empty
            Just hdAddr ->
                let
                    utxo' = Map.singleton txin txout
                    accId = hdAddr ^. hdAddressId . hdAddressIdParent
                in
                    Map.singleton accId (utxo', [hdAddr])

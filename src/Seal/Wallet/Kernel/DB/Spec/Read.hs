{-# LANGUAGE ViewPatterns #-}
-- | Functions on checkpoints
module Seal.Wallet.Kernel.DB.Spec.Read (
    -- * Pure functions on checkpoints
    cpAvailableUtxo
  , cpAvailableBalance
  , cpAvailableGoldBalance
  , cpAvailableDollarBalance
  , cpCheckAvailable
  , cpChange
  , cpTotalBalance
  , cpTotalGoldBalance
  , cpTotalDollarBalance
  , cpTxSlotId
  , cpTxIsPending
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Seal.Chain.Txp as Core
import qualified Seal.Core as Core

import           Seal.Wallet.API.V1.Types (WalAddress (..))
import           Seal.Wallet.Core
import           Seal.Wallet.Kernel.DB.BlockMeta (blockMetaSlotId,
                     localBlockMeta)
import           Seal.Wallet.Kernel.DB.HdWallet
import           Seal.Wallet.Kernel.DB.InDb (fromDb)
import           Seal.Wallet.Kernel.DB.Spec
import qualified Seal.Wallet.Kernel.DB.Spec.Pending as Pending
import           Seal.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Seal.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Seal.Wallet.Kernel.Util.Core as Core

{-------------------------------------------------------------------------------
  Pure functions that support read-only operations on an account Checkpoint, as
  defined in the Wallet Spec
-------------------------------------------------------------------------------}

-- | Available UTxO
--
-- The available UtxO is the current UTxO minus outputs spent by pending txs
cpAvailableUtxo :: IsCheckpoint c => c -> Core.Utxo
cpAvailableUtxo c =
    Core.utxoRemoveInputs (c ^. cpUtxo) pendingIns
  where
    pendingIns = Pending.txIns (c ^. cpPending)

-- | Returns the sets of available and unavailable inputs
cpCheckAvailable :: IsCheckpoint c
                 => Set Core.TxIn -> c -> (Set Core.TxIn, Set Core.TxIn)
cpCheckAvailable ins c = Set.partition isAvailable ins
  where
    isAvailable :: Core.TxIn -> Bool
    isAvailable inp = inp `Map.member` cpAvailableUtxo c

-- | Balance of the available UTxO
cpAvailableBalance :: IsCheckpoint c => c -> Core.Coin
cpAvailableBalance c =
    fromMaybe subCoinErr balance'
  where
    pendingIns   = Pending.txIns $ c ^. cpPending
    spentUtxo    = Core.utxoRestrictToInputs (c ^. cpUtxo) pendingIns
    spentBalance = unsafeIntegerToCoin $ Core.utxoBalance spentUtxo
    balance'     = subCoin (c ^. cpUtxoBalance) spentBalance
    subCoinErr   = error "cpAvailableBalance: spent more than available?"

cpAvailableGoldBalance :: IsCheckpoint c => c -> Core.GoldCoin
cpAvailableGoldBalance c =
    fromMaybe subCoinErr balance'
  where
    pendingIns   = Pending.txIns $ c ^. cpPending
    spentUtxo    = Core.utxoRestrictToInputs (c ^. cpUtxo) pendingIns
    spentBalance = unsafeIntegerToGoldCoin $ Core.utxoGoldBalance spentUtxo
    balance'     = subGoldCoin (c ^. cpUtxoGoldBalance) spentBalance
    subCoinErr   = error "cpAvailableGoldBalance: spent more than available?"

cpAvailableDollarBalance :: IsCheckpoint c => c -> Core.GoldDollar
cpAvailableDollarBalance c =
    fromMaybe subCoinErr balance'
  where
    pendingIns   = Pending.txIns $ c ^. cpPending
    spentUtxo    = Core.utxoRestrictToInputs (c ^. cpUtxo) pendingIns
    spentBalance = unsafeIntegerToGoldDollar $ Core.utxoDollarBalance spentUtxo
    balance'     = subGoldDollar (c ^. cpUtxoDollarBalance) spentBalance
    subCoinErr   = error "cpAvailableBalance: spent more than available?"

-- | Change outputs
--
-- Pending outputs paid back into addresses that belong to the wallet.
cpChange :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.Utxo
cpChange ours cp =
    Map.union
      (Pending.change ours' $ cp ^. cpPending)
      (Pending.change ours' $ cp ^. cpForeign)
  where
    ours' :: Core.Address -> Bool
    ours' addr = IxSet.size (IxSet.getEQ (WalAddress addr) ours) == 1

-- | Total balance (available balance plus change)
cpTotalBalance :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.Coin
cpTotalBalance ours c =
    unsafeAddCoin availableBalance changeBalance
  where
    availableBalance = cpAvailableBalance c
    changeBalance    = unsafeIntegerToCoin $
                         Core.utxoBalance (cpChange ours c)

cpTotalGoldBalance :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.GoldCoin
cpTotalGoldBalance ours c =
    unsafeAddGoldCoin availableGoldBalance changeBalance
  where
    availableGoldBalance = cpAvailableGoldBalance c
    changeBalance        = unsafeIntegerToGoldCoin $
                             Core.utxoGoldBalance (cpChange ours c)

cpTotalDollarBalance :: IsCheckpoint c => IxSet (Indexed HdAddress) -> c -> Core.GoldDollar
cpTotalDollarBalance ours c =
    unsafeAddGoldDollar availableDollarBalance changeBalance
  where
    availableDollarBalance = cpAvailableDollarBalance c
    changeBalance          = unsafeIntegerToGoldDollar $
                             Core.utxoDollarBalance (cpChange ours c)
        
-- | SlotId a transaction got confirmed in
cpTxSlotId :: IsCheckpoint c => Core.TxId -> c -> Maybe Core.SlotId
cpTxSlotId txId c = Map.lookup txId slots
  where
    blockMeta = localBlockMeta (c ^. cpBlockMeta)
    slots     = view (blockMetaSlotId . fromDb) blockMeta

-- | Check if a transaction is pending
cpTxIsPending :: IsCheckpoint c => Core.TxId -> c -> Bool
cpTxIsPending txId c = Pending.member txId (c ^. cpPending)

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Seal.Wallet.Kernel.DB.Compression (
    DeltaCheckpoint (..)
  , DeltaPartialCheckpoint (..)
  , UtxoDiff
  , BlockMetaDiff
  , BlockMetaSlotIdDiff
  , BlockMetaAddressDiff
  ) where

import           Universum

import qualified Data.SafeCopy as SC

import qualified Seal.Chain.Txp as Core
import qualified Seal.Core as Core

import           Seal.Wallet.Kernel.DB.BlockContext
import           Seal.Wallet.Kernel.DB.BlockMeta
import           Seal.Wallet.Kernel.DB.InDb
import           Seal.Wallet.Kernel.DB.Spec.Pending (PendingDiff)
import           UTxO.Util

import           Test.Seal.Core.Arbitrary ()

-- This module aims to help define memory efficient Safecopy instances for Checkpoints.
-- This is done by defining new types, which are the diffs of the Checkpoints.
-- To achieve this we first define diff types for the building blocks of Checkpoint:
-- Map, Pending, BlockMeta and eventually Checkpoint.

data DeltaCheckpoint = DeltaCheckpoint {
    dcUtxo              :: !(InDb UtxoDiff)
  , dcUtxoBalance       :: !(InDb Core.Coin)
  , dcUtxoGoldBalance   :: !(InDb Core.GoldCoin)
  , dcUtxoDollarBalance :: !(InDb Core.GoldDollar)
  , dcPending           :: !PendingDiff
  , dcBlockMeta         :: !BlockMetaDiff
  , dcForeign           :: !PendingDiff
  , dcContext           :: !(Maybe BlockContext)
}

data DeltaPartialCheckpoint = DeltaPartialCheckpoint {
    dcpUtxo               :: !(InDb UtxoDiff)
  , dcpUtxoBalance        :: !(InDb Core.Coin)
  , dcpUtxoGoldBalance    :: !(InDb Core.GoldCoin)
  , dcpUtxoDollarBalance  :: !(InDb Core.GoldDollar)
  , dcpPending            :: !PendingDiff
  , dcpBlockMeta          :: !BlockMetaDiff
  , dcpForeign            :: !PendingDiff
  , dcpContext            :: !BlockContext
}

type UtxoDiff = MapDiff Core.TxIn Core.TxOutAux
type BlockMetaDiff = (BlockMetaSlotIdDiff, BlockMetaAddressDiff)
type BlockMetaSlotIdDiff = InDb (MapDiff Core.TxId Core.SlotId)
type BlockMetaAddressDiff = MapDiff (InDb Core.Address) AddressMeta

instance Differentiable (InDb Core.Utxo) (InDb UtxoDiff) where
  findDelta  = findDeltaUtxo
  applyDelta = applyDeltaUtxo

findDeltaUtxo :: InDb Core.Utxo -> InDb Core.Utxo -> InDb UtxoDiff
findDeltaUtxo inm inm' = findDelta <$> inm <*> inm'

applyDeltaUtxo :: InDb Core.Utxo -> InDb UtxoDiff -> InDb Core.Utxo
applyDeltaUtxo inu dinu = applyDelta <$> inu <*> dinu

instance Differentiable BlockMeta BlockMetaDiff where
  findDelta = findDeltaBlockMeta
  applyDelta = applyDeltaBlockMeta

findDeltaBlockMeta :: BlockMeta -> BlockMeta -> BlockMetaDiff
findDeltaBlockMeta (BlockMeta bmsi bma) (BlockMeta bmsi' bma') =
  (findDelta <$> bmsi <*> bmsi', findDelta bma bma')

applyDeltaBlockMeta :: BlockMeta -> BlockMetaDiff -> BlockMeta
applyDeltaBlockMeta (BlockMeta bmsi bms) (dbmsi, dbms) =
  BlockMeta (applyDelta <$> bmsi <*> dbmsi) (applyDelta bms dbms)

SC.deriveSafeCopy 1 'SC.base ''DeltaCheckpoint
SC.deriveSafeCopy 1 'SC.base ''DeltaPartialCheckpoint

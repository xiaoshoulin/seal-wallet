{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

-- | `Arbitrary` orphan instances

import           Universum

import           Data.Map as M

import           Test.QuickCheck (Arbitrary, Gen, arbitrary)

import qualified Seal.Core as Core
import           Seal.Core.Chrono (NewestFirst (..))

import           Seal.Wallet.Kernel.DB.BlockContext
import           Seal.Wallet.Kernel.DB.BlockMeta
import           Seal.Wallet.Kernel.DB.InDb
import           Seal.Wallet.Kernel.DB.Spec
import           Seal.Wallet.Kernel.DB.Spec.Pending

import           Test.Seal.Chain.Txp.Arbitrary ()
import           Test.Seal.Core.Arbitrary ()

instance Arbitrary BlockContext where
    arbitrary = do
        slotId <- slotIdGen
        hh <- arbitrary
        mhh <- arbitrary
        pure $ BlockContext (InDb slotId) hh mhh

slotIdGen :: Gen Core.SlotId
slotIdGen = do
    w64 <- arbitrary
    w16 <- arbitrary
    return $ Core.SlotId (Core.EpochIndex w64) (Core.UnsafeLocalSlotIndex w16)

instance Arbitrary Checkpoint where
    arbitrary = Checkpoint <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

instance Arbitrary PartialCheckpoint where
    arbitrary = PartialCheckpoint <$> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary

instance Arbitrary c => Arbitrary (Checkpoints c) where
    arbitrary = do
        ls <- arbitrary
        pure . Checkpoints . NewestFirst $ ls

instance Arbitrary BlockMeta where
    arbitrary = do
        n <- arbitrary
        slotsIds <- replicateM n slotIdGen
        txIds <- replicateM n arbitrary
        addrs <- arbitrary
        return $ BlockMeta (InDb . M.fromList $ zip txIds slotsIds) addrs

instance Arbitrary LocalBlockMeta where
    arbitrary = do
        bm <- arbitrary
        pure $ LocalBlockMeta bm

instance Arbitrary AddressMeta where
    arbitrary = AddressMeta <$> arbitrary

instance Arbitrary Pending where
    arbitrary = do
        p <- arbitrary
        return $ fromTransactions p

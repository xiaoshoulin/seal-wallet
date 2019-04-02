{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}

-- | READ queries on the HD wallet
--
-- NOTE: These are pure functions, which are intended to work on a snapshot
-- of the database. They are intended to support the V1 wallet API.
module Seal.Wallet.Kernel.DB.HdWallet.Read (
    -- | Summarize
    accountsByRootId
  , addressesByRootId
  , addressesByAccountId
  , pendingByAccount
  , foreignPendingByAccount
    -- | Simple lookups
  , lookupHdRootId
  , lookupHdAccountId
  , lookupHdAddressId
  , lookupCardanoAddress
    -- | Properties of an entire root
  , rootAssuranceLevel
  , rootTotalBalance
  , rootTotalGoldBalance
  , rootTotalDollarBalance
    -- | Queries on an account's current checkpoint
  , currentUtxo
  , currentAvailableUtxo
  , currentTotalBalance
  , currentTotalGoldBalance
  , currentTotalDollarBalance
  , currentAvailableBalance
  , currentAvailableGoldBalance
  , currentAvailableDollarBalance
  , currentAddressMeta
  , currentTxSlotId
  , currentTxIsPending
  ) where

import           Universum

import           Control.Lens (to)

import           Seal.Chain.Txp (TxId, Utxo)
import           Seal.Core (Address, Coin (..), SlotId, GoldCoin (..),
                     GoldDollar (..))

import           Seal.Wallet.Core        
import           Seal.Wallet.Kernel.DB.BlockMeta (AddressMeta)
import           Seal.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Seal.Wallet.Kernel.DB.HdWallet
import           Seal.Wallet.Kernel.DB.InDb
import           Seal.Wallet.Kernel.DB.Spec (IsCheckpoint (..),
                     cpAddressMeta)
import           Seal.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Seal.Wallet.Kernel.DB.Spec.Read
import           Seal.Wallet.Kernel.DB.Util.AcidState
import           Seal.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Seal.Wallet.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Summarize
-------------------------------------------------------------------------------}

-- | All accounts in the given wallet
--
-- NOTE: Does not check that the root exists.
accountsByRootId :: HdRootId -> Query' e HdWallets (IxSet HdAccount)
accountsByRootId rootId = do
    asks $ IxSet.getEQ rootId . view hdWalletsAccounts

-- | All addresses in the given wallet
--
-- NOTE: Does not check that the root exists.
addressesByRootId :: HdRootId -> Query' e HdWallets (IxSet (Indexed HdAddress))
addressesByRootId rootId =
    asks $ IxSet.getEQ rootId . view hdWalletsAddresses

-- | All addresses in the given account
--
-- NOTE: Does not check that the account exists.
addressesByAccountId :: HdAccountId -> Query' e HdWallets (IxSet (Indexed HdAddress))
addressesByAccountId accId =
    asks $ IxSet.getEQ accId . view hdWalletsAddresses

-- | All pending transactions in all accounts
pendingByAccount :: Query' e HdWallets (Map HdAccountId Pending)
pendingByAccount = fmap aux . IxSet.toMap <$> view hdWalletsAccounts
  where
    aux :: HdAccount -> Pending
    aux acc = acc ^. hdAccountState . hdAccountStateCurrent cpPending

-- | All pending foreign transactions in all accounts
foreignPendingByAccount :: Query' e HdWallets (Map HdAccountId Pending)
foreignPendingByAccount = fmap aux . IxSet.toMap <$> view hdWalletsAccounts
  where
    aux :: HdAccount -> Pending
    aux acc = acc ^. hdAccountState . hdAccountStateCurrent cpForeign


{-------------------------------------------------------------------------------
  Simple lookups
-------------------------------------------------------------------------------}

lookupHdRootId :: HdRootId -> Query' UnknownHdRoot HdWallets HdRoot
lookupHdRootId rootId = zoomHdRootId identity rootId $ ask

lookupHdAccountId :: HdAccountId -> Query' UnknownHdAccount HdWallets HdAccount
lookupHdAccountId accId = zoomHdAccountId identity accId $ ask

lookupHdAddressId :: HdAddressId -> Query' UnknownHdAddress HdWallets HdAddress
lookupHdAddressId addrId = zoomHdAddressId identity addrId $ ask

lookupCardanoAddress :: Address -> Query' UnknownHdAddress HdWallets HdAddress
lookupCardanoAddress addr = zoomHdCardanoAddress identity addr $ ask

{-------------------------------------------------------------------------------
  Properties of an entire HdRoot
-------------------------------------------------------------------------------}

rootAssuranceLevel :: HdRootId -> Query' UnknownHdRoot HdWallets AssuranceLevel
rootAssuranceLevel rootId =
    zoomHdRootId identity rootId $
      view hdRootAssurance

-- | Total balance for all accounts in the given root
--
-- NOTE: Does not check that the root exists.
rootTotalBalance :: HdRootId -> Query' e HdWallets Coin
rootTotalBalance rootId = do
    accounts <- IxSet.getEQ rootId <$> view hdWalletsAccounts
    sumTotals <$> mapM currentTotalBalance' (IxSet.toList accounts)
  where
    sumTotals :: [Coin] -> Coin
    sumTotals = foldl' unsafeAddCoin (mkCoin 0)

rootTotalGoldBalance :: HdRootId -> Query' e HdWallets GoldCoin
rootTotalGoldBalance rootId = do
    accounts <- IxSet.getEQ rootId <$> view hdWalletsAccounts
    sumTotals <$> mapM currentTotalGoldBalance' (IxSet.toList accounts)
  where
    sumTotals :: [GoldCoin] -> GoldCoin
    sumTotals = foldl' unsafeAddGoldCoin (mkGoldCoin 0)

rootTotalDollarBalance :: HdRootId -> Query' e HdWallets GoldDollar
rootTotalDollarBalance rootId = do
    accounts <- IxSet.getEQ rootId <$> view hdWalletsAccounts
    sumTotals <$> mapM currentTotalDollarBalance' (IxSet.toList accounts)
  where
    sumTotals :: [GoldDollar] -> GoldDollar
    sumTotals = foldl' unsafeAddGoldDollar (mkGoldDollar 0)

{-------------------------------------------------------------------------------
  Functions on the most recent checkpoint
-------------------------------------------------------------------------------}

-- | Internal: lift a function on the current checkpoint
liftCP :: (forall c. IsCheckpoint c => c -> a)
       -> HdAccountId -> Query' UnknownHdAccount HdWallets a
liftCP f accId =
    zoomHdAccountId identity accId $
      zoomHdAccountCurrent $
        asks f

-- | Internal: lift a function on the current checkpoints. The difference with
--   @liftCP@ is that we pass downstream a function which combines the current
--   partial and historical checkpoint. This function is only used if the
--   relevant account is under restoration. It also returns the state of the account.
liftCpWithCombine :: (a -> a -> a) -> (forall c. IsCheckpoint c => c -> a)
       -> HdAccountId -> Query' UnknownHdAccount HdWallets (CombinedWithAccountState a)
liftCpWithCombine combine f accId =
    zoomHdAccountId identity accId $
      zoomHdAccountCurrentCombine combine $
        asks f

currentUtxo :: HdAccountId -> Query' UnknownHdAccount HdWallets Utxo
currentUtxo = liftCP (view cpUtxo)

currentAvailableUtxo :: HdAccountId -> Query' UnknownHdAccount HdWallets Utxo
currentAvailableUtxo = liftCP cpAvailableUtxo

currentTxSlotId :: TxId -> HdAccountId -> Query' UnknownHdAccount HdWallets (CombinedWithAccountState (Maybe SlotId))
currentTxSlotId txId = liftCpWithCombine (<|>) (cpTxSlotId txId)

currentTxIsPending :: TxId -> HdAccountId -> Query' UnknownHdAccount HdWallets Bool
currentTxIsPending txId = liftCP $ cpTxIsPending txId

currentAvailableBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets Coin
currentAvailableBalance = liftCP cpAvailableBalance

currentAvailableGoldBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets GoldCoin
currentAvailableGoldBalance = liftCP cpAvailableGoldBalance

currentAvailableDollarBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets GoldDollar
currentAvailableDollarBalance = liftCP cpAvailableDollarBalance

currentAddressMeta :: HdAddress -> Query' UnknownHdAccount HdWallets AddressMeta
currentAddressMeta = withAddr $ \addr ->
    liftCP $ view (cpAddressMeta (addr ^. hdAddressAddress . fromDb))
  where
    withAddr :: (HdAddress -> HdAccountId -> Query' e st a)
             -> (HdAddress -> Query' e st a)
    withAddr f addr = f addr (addr ^. hdAddressId . hdAddressIdParent)

currentTotalBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets Coin
currentTotalBalance accId =
    zoomHdAccountId identity accId ask >>= currentTotalBalance'

currentTotalGoldBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets GoldCoin
currentTotalGoldBalance accId =
    zoomHdAccountId identity accId ask >>= currentTotalGoldBalance'

currentTotalDollarBalance :: HdAccountId -> Query' UnknownHdAccount HdWallets GoldDollar
currentTotalDollarBalance accId =
    zoomHdAccountId identity accId ask >>= currentTotalDollarBalance'

-- Internal helper generalization
--
-- Total balance breaks the pattern because we need the set of addresses that
-- belong to the account, but for that we need 'HdWallets'.
currentTotalBalance' :: HdAccount -> Query' e HdWallets Coin
currentTotalBalance' acc = do
    ourAddrs <- IxSet.getEQ (acc ^. hdAccountId) <$> view hdWalletsAddresses
    return (acc ^. hdAccountState . hdAccountStateCurrent (to $ cpTotalBalance ourAddrs))

currentTotalGoldBalance' :: HdAccount -> Query' e HdWallets GoldCoin
currentTotalGoldBalance' acc = do
    ourAddrs <- IxSet.getEQ (acc ^. hdAccountId) <$> view hdWalletsAddresses
    return (acc ^. hdAccountState . hdAccountStateCurrent (to $ cpTotalGoldBalance ourAddrs))

currentTotalDollarBalance' :: HdAccount -> Query' e HdWallets GoldDollar
currentTotalDollarBalance' acc = do
    ourAddrs <- IxSet.getEQ (acc ^. hdAccountId) <$> view hdWalletsAddresses
    return (acc ^. hdAccountState . hdAccountStateCurrent (to $ cpTotalDollarBalance ourAddrs))

-- | Deal with pending transactions
module Seal.Wallet.Kernel.Pending (
    newPending
  , newForeign
  , cancelPending
  , NewPendingError
  , PartialTxMeta
  ) where

import           Universum hiding (State)

import qualified Data.Map.Strict as Map

import           Control.Concurrent.MVar (modifyMVar_)

import           Data.Acid.Advanced (update')

import           Seal.Chain.Txp (Tx (..), TxAux (..), TxOut (..),
                     AccountOut (..))
import           Seal.Core (Coin (..), GoldCoin (..), GoldDollar (..), 
                     CoinGroup (..), Address, Account (..))
import           Seal.Crypto (EncryptedSecretKey)

import           Seal.Wallet.Kernel.DB.AcidState (CancelPending (..),
                     NewForeign (..), NewForeignError (..), NewPending (..),
                     NewPendingError (..))
import           Seal.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Seal.Wallet.Kernel.DB.HdWallet
import           Seal.Wallet.Kernel.DB.InDb
import qualified Seal.Wallet.Kernel.DB.Spec.Pending as Pending
import           Seal.Wallet.Kernel.DB.TxMeta (TxMeta, putTxMeta)
import           Seal.Wallet.Kernel.Internal
import           Seal.Wallet.Kernel.Read (getFOWallets, getWalletSnapshot)
import           Seal.Wallet.Kernel.Submission (Cancelled, addPending)
import           Seal.Wallet.Kernel.Util.Core

{-------------------------------------------------------------------------------
  Submit pending transactions
-------------------------------------------------------------------------------}

-- | When we create a new Transaction, we don`t yet know which outputs belong to us
-- (it may not be just the change addresses change we create, but also addresses the user specifies).
-- This check happenes in @newTx@. Until then we move around this partial TxMetadata.
-- @Bool@ indicates if all outputs are ours and @Coin@ the sum of the coin of our outputs.
type PartialTxMeta = Bool -> CoinGroup -> TxMeta

-- | Submit a new pending transaction
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newPending :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> PartialTxMeta
           -> IO (Either NewPendingError TxMeta)
newPending w accountId tx partialMeta = do
    newTx w accountId tx partialMeta $ \ourAddrs ->
        update' ((walletPassive w) ^. wallets) $ NewPending accountId (InDb tx) ourAddrs

-- | Submit new foreign transaction
--
-- A foreign transaction is a transaction that transfers funds from /another/
-- wallet to this one.
newForeign :: ActiveWallet
           -> HdAccountId
           -> TxAux
           -> TxMeta
           -> IO (Either NewForeignError ())
newForeign w accountId tx meta = do
    map void <$> newTx w accountId tx (\_ _ ->  meta) $ \ourAddrs ->
        update' ((walletPassive w) ^. wallets) $ NewForeign accountId (InDb tx) ourAddrs

-- | Submit a new transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
--
-- If the transaction is successfully added to the wallet state, transaction metadata
-- is persisted and the submission layer is notified accordingly.
--
-- NOTE: we select "our" output addresses from the transaction and pass it along to the data layer
newTx :: forall e. ActiveWallet
      -> HdAccountId
      -> TxAux
      -> PartialTxMeta
      -> ([HdAddress] -> IO (Either e ())) -- ^ the update to run, takes ourAddrs as arg
      -> IO (Either e TxMeta)
newTx ActiveWallet{..} accountId tx partialMeta upd = do
    snapshot <- getWalletSnapshot walletPassive
    -- run the update
    hdRnds <- getFOWallets walletPassive snapshot

    let allOurAddresses = fst <$> allOurs hdRnds
    res <- upd $ allOurAddresses
    case res of
        Left e   -> return (Left e)
        Right () -> do
            -- process transaction on success
            -- myCredentials should be a list with a single element.
            let thisHdRndRoot = Map.filterWithKey (\hdRoot _ -> accountId ^. hdAccountIdParent == hdRoot) hdRnds
                ourOutputCoins = snd <$> allOurs thisHdRndRoot
                gainedOutputCoins = sumCoinGroupsUnsafe ourOutputCoins
                allOutsOurs = length ourOutputCoins == (length txOut + length txAccountOut)
                txMeta = partialMeta allOutsOurs gainedOutputCoins
            putTxMeta (walletPassive ^. walletMeta) txMeta
            submitTx
            return (Right txMeta)
    where
        (txOut :: [TxOut]) = _txUtxoOutputs . taTx $ tx

        (txAccountOut :: [AccountOut]) = _txAccountOutputs . taTx $ tx

        -- | NOTE: we recognise addresses in the transaction outputs that belong to _all_ wallets,
        --  not only for the wallet to which this transaction is being submitted
        allOurs
            :: Map HdRootId EncryptedSecretKey
            -> [(HdAddress, CoinGroup)]
        allOurs = ourTxOuts <> ourAccountOuts
          where
            ourTxOuts = evalState $ fmap catMaybes $ forM (map resolveTxOut txOut) $ 
                 \(addr, cg) -> fmap (, cg) <$> state (isOurs addr)

            ourAccountOuts = evalState $ fmap catMaybes $ forM txAccountOut $
                  \DepositOutput {..} -> do
                    fmap (, doValue) <$> state (isOurs (getAccount doAccount))

        resolveTxOut :: TxOut -> (Address, CoinGroup)
        resolveTxOut TxOutSeal {..} = (txOutAddress, CoinGroup txOutSeal (GoldCoin 0) (GoldDollar 0))
        resolveTxOut TxOutGold {..} = (txOutAddress, CoinGroup (Coin 0) txOutGold (GoldDollar 0))
        resolveTxOut TxOutDollar {..} = (txOutAddress, CoinGroup (Coin 0) (GoldCoin 0) txOutDollar)
        resolveTxOut TxOutState {..} = (txOutAddress, CoinGroup (Coin 0) (GoldCoin 0) (GoldDollar 0))

        submitTx :: IO ()
        submitTx = modifyMVar_ (walletPassive ^. walletSubmission) $
                    return . addPending accountId (Pending.singleton tx)

-- | Cancel a pending transaction
--
-- NOTE: This gets called in response to events /from/ the wallet submission
-- layer, so we shouldn't be notifying the submission in return here.
--
-- This removes the transaction from either pending or foreign.
cancelPending :: PassiveWallet -> Cancelled -> IO ()
cancelPending passiveWallet cancelled =
    update' (passiveWallet ^. wallets) $ CancelPending (fmap InDb cancelled)

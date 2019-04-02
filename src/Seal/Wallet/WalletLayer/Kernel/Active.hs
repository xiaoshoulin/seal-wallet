{-# LANGUAGE LambdaCase #-}

module Seal.Wallet.WalletLayer.Kernel.Active (
    pay
  , estimateFees
  , createUnsignedTx
  , submitSignedTx
  ) where

import qualified Serokell.Util.Base16 as B16
import           Universum

import           Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Second)

import           Seal.Binary.Class (decodeFull')
import           Seal.Chain.Genesis
import           Seal.Chain.Txp (Tx (..), TxSigData (..))
import           Seal.Core (Address (..), Coin (..), GoldCoin (..),
                     GoldDollar (..), SlotId (..), TxFeePolicy)
import           Seal.Crypto (PublicKey, Signature (..))

import           Seal.Crypto.Wallet (xsignature)
import qualified Seal.Wallet.API.V1.Types as V1
import qualified Seal.Wallet.Kernel as Kernel
import           Seal.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation,
                     InputGrouping, newOptions)
import qualified Seal.Wallet.Kernel.DB.HdWallet as HD
import           Seal.Wallet.Kernel.DB.TxMeta.Types
-- import           Seal.Wallet.Kernel.Internal (walletProtocolMagic)
import qualified Seal.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Seal.Wallet.Kernel.Transactions as Kernel
import           Seal.Wallet.WalletLayer (EstimateFeesError (..),
                     NewPaymentError (..), NewUnsignedTransactionError (..),
                     SubmitSignedTransactionError (..))
import           Seal.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Seal.Wallet.WalletLayer.Kernel.Conv

-- | Generates a new transaction @and submit it as pending@.
pay :: MonadIO m
    => Kernel.ActiveWallet
    -> Config
    -> InputGrouping
    -> ExpenseRegulation
    -> V1.Payment
    -> m (Either NewPaymentError (Tx, TxMeta))
pay activeWallet config grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $
      runExceptT $ do
        (_, accId, payees, payeesgold, payeesdollar) <- withExceptT NewPaymentWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        -- Verify that all payee addresses are of the same `NetworkMagic`
        -- as our `ActiveWallet`.
        --  nm = makeNetworkMagic $ Kernel.walletPassive activeWallet ^. walletProtocolMagic
        let    pw = maybe mempty coerce $ V1.pmtSpendingPassword payment
        let mRemarks = V1.pmtRemark payment
        -- ExceptT $ pure $ verifyPayeesNM nm payees
        let aouts = map V1.disToAccountOut $ V1.pmtDestinationaccounts payment
        let cmds = map V1.toCmd $ V1.pmtCmds payment
        -- Pay the payees
        withExceptT NewPaymentError $ ExceptT $
            Kernel.pay activeWallet config mRemarks pw accId payees payeesgold payeesdollar aouts cmds


-- | Verifies that the `NetworkMagic` of each payee address matches the
-- provided `NetworkMagic`.
-- verifyPayeesNM
--     :: NetworkMagic
--     -> NonEmpty (Address, Coin, Maybe SlotId)
--     -> Either NewPaymentError ()
-- verifyPayeesNM nm payees =
--     case nonEmpty invalidPayees of
--         Nothing -> Right ()
--         Just is -> Left $ NewPaymentAddressBadNetworkMagic nm is
--   where
--     addressHasValidMagic :: AddrAttributes -> Bool
--     addressHasValidMagic addrAttrs = nm == (aaCategory addrAttrs)
--     --
--     verifyPayeeNM
--         :: (Address, Coin, Maybe SlotId)
--         -> Either Address ()
--     verifyPayeeNM (addr, _)
--         | (addressHasValidMagic ((attrData . addrAttributes) addr)) = Right ()
--         | otherwise = Left addr
--     --
--     invalidPayees :: [Address]
--     invalidPayees = fst $ partitionEithers (toList (map verifyPayeeNM payees))

-- | Estimates the fees for a payment.
estimateFees :: MonadIO m
             => Kernel.ActiveWallet
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> m (Either EstimateFeesError Coin)
estimateFees activeWallet grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
      runExceptT $ do
        (opts, accId, payees, payeesgold, payeesdollar) <- withExceptT EstimateFeesWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        let aouts = map V1.disToAccountOut $ V1.pmtDestinationaccounts payment
        let cmds = map V1.toCmd $ V1.pmtCmds payment
        withExceptT EstimateFeesError $ ExceptT $
          Kernel.estimateFees activeWallet opts accId payees payeesgold payeesdollar aouts cmds

-- | Creates a raw transaction.
--
-- NOTE: this function does /not/ perform a payment, it just creates a new
-- transaction which will be signed and submitted to the blockchain later.
-- It returns a transaction and a list of source addresses with corresponding
-- derivation paths.
createUnsignedTx :: MonadIO m
                 => Kernel.ActiveWallet
                 -> InputGrouping
                 -> ExpenseRegulation
                 -> V1.Payment
                 -> m (Either NewUnsignedTransactionError V1.UnsignedTransaction)
createUnsignedTx activeWallet grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    let spendingPassword = maybe mempty coerce $ V1.pmtSpendingPassword payment
    res <- runExceptT $ do
        (opts, accId, payees, payeesgold, payeesdollar) <- withExceptT NewTransactionWalletIdDecodingFailed $
            setupPayment policy grouping regulation payment
        let aouts = map V1.disToAccountOut $ V1.pmtDestinationaccounts payment
        let cmds = map V1.toCmd $ V1.pmtCmds payment
        withExceptT NewUnsignedTransactionError $ ExceptT $
            Kernel.prepareUnsignedTxWithSources activeWallet
                                                opts
                                                accId
                                                payees
                                                payeesgold
                                                payeesdollar
                                                aouts
                                                cmds
                                                spendingPassword
    case res of
        Left e -> return $ Left e
        Right (tx, addrsAndPaths) -> do
            let txInHexFormat = V1.mkTransactionAsBase16 tx
                srcAddrsWithDerivationPaths = 
                    map (\(addr, path) -> V1.AddressAndPath (V1.WalAddress addr)
                                                               (map V1.word32ToAddressLevel path))
                           addrsAndPaths
            return $ Right $ V1.UnsignedTransaction txInHexFormat
                                                    srcAddrsWithDerivationPaths

-- | Submits externally-signed transaction to the blockchain.
submitSignedTx :: MonadIO m
               => Kernel.ActiveWallet
               -> V1.SignedTransaction
               -> m (Either SubmitSignedTransactionError (Tx, TxMeta))
submitSignedTx activeWallet (V1.SignedTransaction encodedTx encodedSrcAddrsWithProofs) = runExceptT $ do
    txAsBytes <- withExceptT (const SubmitSignedTransactionNotBase16Format) $ ExceptT $
        pure $ B16.decode (V1.rawTransactionAsBase16 encodedTx)
    tx :: Tx <- withExceptT (const SubmitSignedTransactionUnableToDecode) $ ExceptT $
        pure $ decodeFull' txAsBytes

    srcAddrsWithProofs <- mapM decodeAddrAndProof encodedSrcAddrsWithProofs
    let problems = lefts srcAddrsWithProofs
    if not . null $ problems then
        -- Something is wrong with proofs, take the first problem we know about.
        let (firstProblem:_) = problems in
        ExceptT $ pure $ Left firstProblem
    else
        let validSrcAddrsWithProofs = rights srcAddrsWithProofs in
        withExceptT SubmitSignedTransactionError $ ExceptT $ liftIO $
            Kernel.submitSignedTx activeWallet
                                  tx
                                  (NE.fromList validSrcAddrsWithProofs)
  where
    decodeAddrAndProof :: Monad m
                       => V1.AddressWithProof
                       -> m (Either SubmitSignedTransactionError (Address, Signature TxSigData, PublicKey))
    decodeAddrAndProof (V1.AddressWithProof srcAddr encSig derivedPK) = runExceptT $ do
        txSigItself <- withExceptT (const SubmitSignedTransactionSigNotBase16Format) $ ExceptT $
            pure $ B16.decode (V1.rawTransactionSignatureAsBase16 encSig)
        realTxSig <- withExceptT (const SubmitSignedTransactionInvalidSig) $ ExceptT $
            pure $ xsignature txSigItself
        let txSignature = Signature realTxSig :: Signature TxSigData
            V1.WalAddress rawSrcAddr = srcAddr
        ExceptT $ pure $ Right (rawSrcAddr, txSignature, derivedPK)


-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: Monad m
             => TxFeePolicy
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> ExceptT Text m ( CoinSelectionOptions
                               , HD.HdAccountId
                               , [(Address, Coin, Maybe SlotId)]
                               , [(Address, GoldCoin)]
                               , [(Address, GoldDollar)]
                               )
setupPayment policy grouping regulation payment = do
    rootId <- fromRootId wId
    let opts   = (newOptions (Kernel.cardanoFee policy)) {
                     csoExpenseRegulation = regulation
                   , csoInputGrouping     = grouping
                   }
        accIx  = HD.HdAccountIx (V1.getAccIndex . V1.psAccountIndex . V1.pmtSource $ payment)
        accId  = HD.HdAccountId {
                     _hdAccountIdParent = rootId
                   , _hdAccountIdIx     = accIx
                   }
        payees = (\(V1.PaymentDistribution a c l) -> (V1.unWalAddress a, V1.unWalletCoin c, V1.unWalSlotId l)) <$>
                   V1.pmtDestinations payment
        payeesgold = (\(V1.GoldPaymentDistribution a c) -> (V1.unWalAddress a, V1.unWalGoldCoin c)) <$>
                   V1.pmtDestinationgolds payment
        payeesdollar = (\(V1.DollarPaymentDistribution a c) -> (V1.unWalAddress a, V1.unWalGoldDollar c)) <$>
                   V1.pmtDestinationdollars payment
    return (opts, accId, payees, payeesgold, payeesdollar)
  where
    wId = V1.psWalletId . V1.pmtSource $ payment

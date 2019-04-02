{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
module Seal.Wallet.Kernel.Transactions (
      pay
    , estimateFees
    -- * Errors
    , NewTransactionError(..)
    , SignTransactionError(..)
    , PaymentError(..)
    , EstimateFeesError(..)
    , NumberOfMissingUtxos(..)
    , NumberOfZeroAmountOutputs(..)
    , cardanoFee
    , mkStdTx
    , prepareUnsignedTxWithSources
    , submitSignedTx
    -- * Internal & testing use only low-level APIs
    , newTransaction
    , newUnsignedTransaction
    , toMeta
  ) where

import           Universum 

import           Control.Exception
import           Control.Lens (to)
import           Control.Monad.Except (withExceptT)
import           Control.Retry (RetryPolicyM, RetryStatus, applyPolicy,
                     fullJitterBackoff, limitRetries, retrying)
import           Crypto.Random (MonadRandom (..))
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import           Data.Default (def)
import qualified Seal.Wallet.Kernel.DB.Util.IxSet as IxSet
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified System.Random.MWC (GenIO, asGenIO, initialize, uniformVector)
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable

-- import           Seal.Core.Util.LogSafe (SecureLog (..))
import           Seal.Crypto.Wallet (DerivationIndex)
import           Seal.Chain.Genesis as Genesis
import qualified Seal.Wallet.Kernel.Addresses as Kernel
import           Seal.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelFinalResult (..), CoinSelectionOptions (..),
                     estimateCardanoFee, estimateMaxTxInputs)
import qualified Seal.Wallet.Kernel.CoinSelection.FromGeneric as CoinSelection
import           Seal.Wallet.Kernel.CoinSelection.Generic
                     (CoinSelHardErr (..))
import           Seal.Wallet.Kernel.DB.AcidState (DB (..), NewPendingError)
import           Seal.Wallet.Kernel.DB.HdWallet
import qualified Seal.Wallet.Kernel.DB.HdWallet as HD
import           Seal.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Seal.Wallet.Kernel.DB.Util.IxSet (Indexed (..))
import           Seal.Wallet.Kernel.DB.Read as Getters
-- import           Seal.Wallet.Kernel.DB.Spec
import           Seal.Wallet.Kernel.DB.TxMeta.Types
import           Seal.Wallet.Kernel.Ed25519Bip44
                     (ChangeChain (ExternalChain), deriveAddressKeyPair)
import           Seal.Wallet.Kernel.Internal (ActiveWallet (..),
                     PassiveWallet (..), walletNode)
import qualified Seal.Wallet.Kernel.Internal as Internal
import qualified Seal.Wallet.Kernel.Keystore as Keystore
import qualified Seal.Wallet.Kernel.NodeStateAdaptor as Node
import           Seal.Wallet.Kernel.Pending (PartialTxMeta, newPending)
import           Seal.Wallet.Kernel.Read (getWalletSnapshot)
import           Seal.Wallet.Kernel.Types (RawResolvedTx (..))
import           Seal.Wallet.Kernel.Util.Core
import           Seal.Wallet.WalletLayer.Kernel.Conv (exceptT)
import           Seal.Chain.Txp (Tx (..), TxAttributes, TxAux (..), TxId,
                     TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxSigData (..), Utxo, AccountIn (..), AccountOut (..), Nonce)
import           Seal.Chain.Txp as Core (TxAttributes, TxAux, TxIn, TxOut,
                     TxOutAux, toaOut, txOutAddress, txOutSeal)
import qualified Seal.Client.Txp.Util as CTxp
import           Seal.Client.Txp.Network (prepareMTx)
import           Seal.Core (Address, Coin, GoldCoin, GoldDollar, SlotId, TxFeePolicy (..))
import qualified Seal.Core as Core
import           Seal.Core.Attributes (Attributes (attrData))
import           Seal.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Seal.Crypto (EncryptedSecretKey, PassPhrase, ProtocolMagic,
                     PublicKey, SafeSigner (..), 
                     ShouldCheckPassphrase (..), Signature (..), hash)
import           Seal.Wallet.Core (unsafeSubCoin, mkCoin, unsafeAddCoin)
-- import           UTxO.Util (shuffle)
{-------------------------------------------------------------------------------
  Generating payments and estimating fees
-------------------------------------------------------------------------------}
data NumberOfMissingUtxos = NumberOfMissingUtxos Int

instance Buildable NumberOfMissingUtxos where
    build (NumberOfMissingUtxos number) =
        bprint ("NumberOfMissingUtxos " % build) number

instance Arbitrary NumberOfMissingUtxos where
    arbitrary = oneof [ NumberOfMissingUtxos <$> arbitrary
                      ]

data NumberOfZeroAmountOutputs = NumberOfZeroAmountOutputs Int

instance Buildable NumberOfZeroAmountOutputs where
    build (NumberOfZeroAmountOutputs number) =
        bprint ("NumberOfZeroAmountOutputs " % build) number

instance Arbitrary NumberOfZeroAmountOutputs where
    arbitrary = oneof [ NumberOfZeroAmountOutputs <$> arbitrary
                      ]


data NewTransactionError =
    NewTransactionUnknownAccount UnknownHdAccount
  | NewTransactionUnknownAddress UnknownHdAddress
  | NewTransactionErrorCoinSelectionFailed CoinSelHardErr
  | NewTransactionErrorCreateAddressFailed Kernel.CreateAddressError
  | NewTransactionErrorSignTxFailed SignTransactionError
  | NewTransactionInvalidTxIn
  | NewTransactionNotEnoughUtxoFragmentation NumberOfMissingUtxos
  | NewTransactionZeroAmountCoin NumberOfZeroAmountOutputs

instance Buildable NewTransactionError where
    build (NewTransactionUnknownAccount err) =
        bprint ("NewTransactionUnknownAccount " % build) err
    build (NewTransactionUnknownAddress err) =
        bprint ("NewTransactionUnknownAddress " % build) err
    build (NewTransactionErrorCoinSelectionFailed err) =
        bprint ("NewTransactionErrorCoinSelectionFailed " % build) err
    build (NewTransactionErrorCreateAddressFailed err) =
        bprint ("NewTransactionErrorCreateAddressFailed " % build) err
    build (NewTransactionErrorSignTxFailed err) =
        bprint ("NewTransactionErrorSignTxFailed " % build) err
    build NewTransactionInvalidTxIn =
        bprint "NewTransactionInvalidTxIn"
    build (NewTransactionNotEnoughUtxoFragmentation err) =
        bprint ("NewTransactionNotEnoughUtxoFragmentation" % build) err
    build (NewTransactionZeroAmountCoin err) =
        bprint ("NewTransactionZeroAmountCoin" % build) err

instance Arbitrary NewTransactionError where
    arbitrary = oneof [
        NewTransactionUnknownAccount <$> arbitrary
      , NewTransactionErrorCoinSelectionFailed <$> oneof
            [ pure $ CoinSelHardErrUtxoExhausted "0 coin(s)" "14 coin(s)"
            , pure CoinSelHardErrCannotCoverFee
            ]
      , NewTransactionErrorCreateAddressFailed <$> arbitrary
      , NewTransactionErrorSignTxFailed <$> arbitrary
      , pure NewTransactionInvalidTxIn
      , NewTransactionNotEnoughUtxoFragmentation <$> arbitrary
      , NewTransactionZeroAmountCoin <$> arbitrary
      ]

data PaymentError = PaymentNewTransactionError NewTransactionError
                  | PaymentNewPendingError NewPendingError
                  | PaymentSubmissionMaxAttemptsReached
                  -- ^ When trying to send the newly-created transaction via
                  -- 'newPending' and the submission layer, we hit the number
                  -- of retries/max time allocated for the operation.
                  | PaymentNoHdAddressForSrcAddress HD.UnknownHdAddress
                  -- ^ When we don't have HD-address corresponding to source
                  -- address of transaction.

instance Buildable PaymentError where
    build (PaymentNewTransactionError txErr) =
        bprint ("PaymentNewTransactionError " % build) txErr
    build (PaymentNewPendingError npe) =
        bprint ("PaymentNewPendingError " % build) npe
    build PaymentSubmissionMaxAttemptsReached =
        bprint "PaymentSubmissionMaxAttemptsReached"
    build (PaymentNoHdAddressForSrcAddress addrErr) =
        bprint ("PaymentNoHdAddressForSrcAddress" % build) addrErr

-- | Workhorse kernel function to perform a payment. It includes logic to
-- stop trying to perform a payment if the payment would take more than 30
-- seconds, as well as internally retrying up to 5 times to propagate the
-- transaction via 'newPending'.
pay :: ActiveWallet
    -> Config
    -> Maybe Text
    -> PassPhrase
    -> HdAccountId
    -- ^ The source HD Account from where the payment was originated
    -> [(Address, Coin, Maybe SlotId)]
    -> [(Address, GoldCoin)]
    -> [(Address, GoldDollar)]
    -- ^ The payees
    -> [AccountOut]
    -> [Core.Cmd]

    -> IO (Either PaymentError (Tx, TxMeta))
pay activeWallet config mRemarks spendingPassword accountId payees payeesgold payeesdollar aouts cmds = do
    retrying retr shouldRetry $ \rs -> do
        res <- newTransaction activeWallet config mRemarks spendingPassword accountId payees payeesgold payeesdollar aouts cmds
        case res of
             Left e      -> return (Left $ PaymentNewTransactionError e)
             Right (txAux, partialMeta, _utxo) -> do
                 succeeded <- newPending activeWallet accountId txAux partialMeta
                 case succeeded of
                      Left e   -> do
                          -- If the next retry would bring us to the
                          -- end of our allowed retries, we fail with
                          -- a proper error
                          retriesLeft <- applyPolicy retr rs
                          return . Left $ case retriesLeft of
                               Nothing ->
                                   PaymentSubmissionMaxAttemptsReached
                               Just _  ->
                                   PaymentNewPendingError e
                      Right meta -> return $ Right (taTx $ txAux, meta)

-- See <https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter>
retr :: RetryPolicyM IO
retr = fullJitterBackoff 5000000 <> limitRetries 6

-- If this is a hard coin selection error we cannot recover, stop
-- retrying. If we get a 'Tx' as output, stop retrying immediately.
shouldRetry :: RetryStatus -> Either PaymentError a -> IO Bool
shouldRetry _ (Right _)                             = return False
shouldRetry _ (Left (PaymentNewTransactionError _)) = return False
shouldRetry _ _                                     = return True

{-----------------------------------------------------------------------------
  Creating transactions (low-level API)
------------------------------------------------------------------------------}

-- | Creates a new unsigned 'Tx', without submitting it to the network. Please
-- note that this is a low-level API. Considering using 'pay' and 'estimateFee'
-- in user's code.
--
-- For testing purposes, if successful this additionally returns the utxo
-- that coin selection was run against.
newUnsignedTransaction
    :: ActiveWallet
    -> CoinSelectionOptions
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate
    -> [(Address, Coin, Maybe SlotId)]
    -> [(Address, GoldCoin)]
    -> [(Address, GoldDollar)]
    -- ^ The payees
    -> [AccountOut]
    -> [Core.Cmd]
    -> IO (Either NewTransactionError (DB, UnsignedTx, Coin, Utxo))
    -- ^ Returns the state of the world (i.e. the DB snapshot)
    -- at the time of the coin selection, so that it can later
    -- on be used to sign the addresses.
newUnsignedTransaction ActiveWallet{..} options accountId payees payeesgold payeesdollar aouts cmds = runExceptT $ do
    snapshot <- liftIO $ getWalletSnapshot walletPassive
    initialEnv <- liftIO $ newEnvironment
    maxTxSize  <- liftIO $ Node.getMaxTxSize (walletPassive ^. walletNode)
    -- TODO: We should cache this maxInputs value
    let maxInputs = estimateMaxTxInputs maxTxSize

    -- STEP 0: Get available UTxO
    availableUtxo <- withExceptT NewTransactionUnknownAccount $ exceptT $
                       currentAvailableUtxo snapshot accountId

    withExceptT NewTransactionNotEnoughUtxoFragmentation $ exceptT $
        checkUtxoFragmentation payees availableUtxo

    withExceptT NewTransactionZeroAmountCoin $ exceptT $
        checkCoins payees

    -- STEP 1: Run coin selection.
    CoinSelFinalResult inputs outputs coins <-
      withExceptT NewTransactionErrorCoinSelectionFailed $ ExceptT $
        flip runReaderT initialEnv . buildPayment $
          CoinSelection.random options
                               maxInputs
                               (toTxOuts payees payeesgold payeesdollar)
                               availableUtxo

    -- STEP 2: Assemble the unsigned transactions, @without@ generating the
    -- change addresses, as that would require the spending password.
    -- Currently all transactions has default (empty) attributes. Please note
    -- that it may change in the future.
    let attributes = def :: TxAttributes
    let tx = UnsignedTx inputs outputs [] aouts cmds attributes coins
    let fees = computeFeesOfUnsignedTx tx
    return (snapshot, tx, fees, availableUtxo)
  where
    -- Generate an initial seed for the random generator using the hash of
    -- the payees, which ensure that the coin selection (and the fee estimation)
    -- is \"pseudo deterministic\" and replicable.
    newEnvironment :: IO Env
    newEnvironment =
        let initialSeed = V.fromList . map fromIntegral
                                     . B.unpack
                                     . encodeUtf8 @Text @ByteString
                                     . sformat build
                                     $ hash payees
        in Env <$> System.Random.MWC.initialize initialSeed

    checkUtxoFragmentation
        :: [(Address, Coin, Maybe SlotId)]
        -> Utxo
        -> Either NumberOfMissingUtxos ()
    checkUtxoFragmentation outputs inputs =
        let numberOfUtxo = Map.size inputs
            numberOfOutputs = length outputs
            diff = numberOfOutputs - numberOfUtxo
        in if diff > 0 then
            Left $ NumberOfMissingUtxos diff
           else
            Right ()

    checkCoins
        :: [(Address, Coin, Maybe SlotId)]
        -> Either NumberOfZeroAmountOutputs ()
    checkCoins outputs =
        let numberOfZeroAmountOutputs = length
                                      $ filter (== 0)
                                      $ map (Core.getCoin . ssnd) outputs
        in if numberOfZeroAmountOutputs > 0 then
            Left $ NumberOfZeroAmountOutputs numberOfZeroAmountOutputs
           else
            Right ()
        where
            ssnd :: (a, b, c) -> b
            ssnd (_, b, _) = b


-- | Creates a new unsigned transaction.
--
-- NOTE: this function does /not/ perform a payment, it just creates a new
-- transaction which will be signed and submitted to the blockchain later.
-- It returns a transaction and a list of source addresses with corresponding
-- derivation paths.
prepareUnsignedTxWithSources
    :: ActiveWallet
    -> CoinSelectionOptions
    -- ^ The options describing how to tune the coin selection.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> [(Address, Coin, Maybe SlotId)] 
    -> [(Address, GoldCoin)]
    -> [(Address, GoldDollar)]
    -- ^ The payees.
    -> [AccountOut]
    -> [Core.Cmd]

    -> PassPhrase
    -> IO (Either
             NewTransactionError
             (Tx, [(Address, [DerivationIndex])])
          )
prepareUnsignedTxWithSources activeWallet opts srcAccountId payees payeesgold payeesdollar aouts cmds spendingPassword = runExceptT $ do
    (db, unsignedTx, _fees, _availableUtxo) <- ExceptT $
        newUnsignedTransaction activeWallet opts srcAccountId payees payeesgold payeesdollar aouts cmds

    -- Now we have to generate the change addresses needed,
    -- because 'newUnsignedTransaction' function cannot do it by itself.
    changeAddresses <- withExceptT NewTransactionErrorCreateAddressFailed $
        genChangeOuts (unsignedTxChange unsignedTx)
                      srcAccountId
                      spendingPassword
                      (walletPassive activeWallet)
    -- We have to provide source addresses and derivation paths for this transaction.
    -- It will be used by external party to provide a proof that it has a right to
    -- spend this money.
    let tx = UnsafeTx
              (map fst $ unsignedTxInputs unsignedTx)
              (map toaOut $ appendList (unsignedTxOutputs unsignedTx) changeAddresses)
              [] aouts cmds
              (unsignedTxAttributes unsignedTx)
    ExceptT $ return $ case mapM (prepareSourceAddress db) $ unsignedTxInputs unsignedTx of
        Left err  -> Left err
        Right res -> Right $ (tx, res)
  where
    appendList :: [a] -> [a] -> [a]
    appendList a b = a <>b

    prepareSourceAddress
        :: DB -> (Core.TxIn, Core.TxOutAux)
        -> Either NewTransactionError (Address, [DerivationIndex])
    prepareSourceAddress db addr
        = case lookupCardanoAddress db address of
            Left  err -> Left $ NewTransactionUnknownAddress err
            Right res -> Right (address, [accountIndex, getAddressIndex res])
      where
        address = txOutAddress $ toaOut $ snd addr
        accountIndex = getHdAccountIx  $ _hdAccountIdIx srcAccountId
        getAddressIndex = getHdAddressIx . _hdAddressIdIx . _hdAddressId

-- | Submits externally-signed transaction to the blockchain.
-- The result of this function is equal to the result of 'pay' function.
submitSignedTx
    :: ActiveWallet
    -> Tx
    -> NonEmpty (Address, Signature TxSigData, PublicKey)
    -> IO (Either PaymentError (Tx, TxMeta))
submitSignedTx aw@ActiveWallet{..} tx srcAddrsWithProofs =
    retrying retr shouldRetry $ \rs -> do
        res <- runExceptT $ do
            -- STEP 0: get wallet snapshot.
            snapshot <- liftIO $ getWalletSnapshot walletPassive
            -- STEP 1: create witnesses.
            -- Since we already received inputs signatures with corresponding derived PKs,
            -- just form witnesses from them.
            let witnesses = V.fromList . NonEmpty.toList $ flip NonEmpty.map srcAddrsWithProofs $
                    \(_srcAddr, txSig, derivedPK) -> PkWitness derivedPK txSig

            -- STEP 2: make 'TxAux'.
            let txAux = TxAux tx witnesses

            -- STEP 3: Compute metadata
            let txId = hash tx
            -- Currently it's assumed that all source addresses belong to 'the same/ account,
            -- so we can just take the first source address to find our 'HdAccountId'.
            let (firstSrcAddress, _, _) = NonEmpty.head srcAddrsWithProofs
            firstSrcHdAddress <- withExceptT PaymentNoHdAddressForSrcAddress $ exceptT $
                lookupCardanoAddress snapshot firstSrcAddress
            let (HD.HdAddress (HD.HdAddressId srcAccountId _) _) = firstSrcHdAddress
            -- We use `getCreationTimestamp` provided by the `NodeStateAdaptor`
            -- to compute the createdAt timestamp for `TxMeta`.
            txMetaCreatedAt_ <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)

            -- STEP 4: Get available UTxO
            utxo <- withExceptT (PaymentNewTransactionError . NewTransactionUnknownAccount) $ exceptT $
                currentAvailableUtxo snapshot srcAccountId

            let inputs = _txUtxoInputs tx
                maybeInputsWithCoins = map (collectInputCoins utxo) inputs
                inputsWithCoins = catMaybes maybeInputsWithCoins
            -- If 'tx' is valid (i.e. contains correct inputs), we already have
            -- all inputs with corresponding coins.
            let numberOfValidInputs = length inputsWithCoins
                numberOfAllInputs = length maybeInputsWithCoins
            if numberOfValidInputs /= numberOfAllInputs then
                -- Something is wrong with inputs, this 'tx' should be rejected.
                ExceptT $ return $ Left $ PaymentNewTransactionError NewTransactionInvalidTxIn
            else do
                -- We have to calculate the sum of input coins.
                let spentInputCoinGroups = paymentAmount (_txAccountInputs tx) (map (toaOut . snd) inputsWithCoins)

                let ainputs = map toResolvedTxAccountIn (_txAccountInputs tx)
                let aoutputs = map toResolvedTxAccountOut (_txAccountOutputs tx)

                -- STEP 5: form meta-data.
                partialMeta <- liftIO $ createNewMeta srcAccountId
                                                      txId
                                                      txMetaCreatedAt_
                                                      inputsWithCoins
                                                      (_txUtxoOutputs tx)
                                                      ainputs
                                                      aoutputs
                                                      (_txCmds tx)
                                                      True
                                                      spentInputCoinGroups
                -- STEP 6: our new pending tx.
                withExceptT PaymentNewPendingError $ ExceptT $
                    newPending aw srcAccountId txAux partialMeta
        case res of
            Left e -> do
                -- If the next retry would bring us to the end of our allowed retries,
                -- we fail with a proper error
                retriesLeft <- applyPolicy retr rs
                return $ case retriesLeft of
                    Nothing -> Left PaymentSubmissionMaxAttemptsReached
                    Just _  -> Left e
            Right meta ->
                return $ Right (tx, meta)
  where
    -- If utxo is valid, we definitely know that .
    collectInputCoins :: Utxo
                      -> TxIn
                      -> Maybe (TxIn, TxOutAux)
    collectInputCoins utxo txInput = case Map.lookup txInput utxo of
        Nothing       -> Nothing
        Just txOutput -> Just (txInput, txOutput)
    
toResolvedTxAccountIn :: AccountIn -> (Core.Address, Nonce, Core.CoinGroup)
toResolvedTxAccountIn ai = (Core.getAccount $ riAccount ai, riNonce ai, riValue ai)

toResolvedTxAccountOut :: AccountOut -> (Core.Address, Core.CoinGroup)
toResolvedTxAccountOut ao = (Core.getAccount $ doAccount ao, doValue ao)
-- | Creates a new 'TxAux' and corresponding 'TxMeta',
-- without submitting it to the network.
--
-- For testing purposes, if successful this additionally returns the utxo
-- that coin selection was run against.
newTransaction
    :: ActiveWallet
    -> Config
    -> Maybe Text
    -> PassPhrase
    -- ^ The spending password.
    -> HdAccountId
    -- ^ The source HD account from where the payment should originate.
    -> [(Address, Coin, Maybe SlotId)]
    -> [(Address, GoldCoin)]
    -> [(Address, GoldDollar)]
    -- ^ The payees.
    -> [AccountOut]
    -> [Core.Cmd]

    -> IO (Either NewTransactionError (TxAux, PartialTxMeta, Utxo))
newTransaction ActiveWallet{..} config mRemarks spendingPassword accountId payees payeesgold payeesdollar aouts cmds = do   
    let pm = walletPassive ^. Internal.walletProtocolMagic
        nm = makeNetworkMagic pm
    db <- liftIO $ getWalletSnapshot walletPassive
    mbEsk <- liftIO $ Keystore.lookup
                nm
                (accountId ^. hdAccountIdParent)
                (walletPassive ^. Internal.walletKeystore)        
    let 
        getSigner :: Address -> Either SignTransactionError SafeSigner
        getSigner = mkSigner nm spendingPassword mbEsk db
    
    let toSigner :: Address -> Maybe SafeSigner
        toSigner addr = case getSigner addr of
            Left _ -> Nothing
            Right signer -> Just signer

    let hdAddress = map _ixedIndexed $ IxSet.toList $ _hdWalletsAddresses $ _dbHdWallets db
    let srcAddrs = map _fromDb $ map _hdAddressAddress hdAddress
    let outputs = toTxOuts payees payeesgold payeesdollar
    changeAddresses <- Kernel.createAddress spendingPassword accountId walletPassive
    let changeaddr = fromRight (List.head srcAddrs) changeAddresses
    
    let availableUtxo = currentAvailableUtxo db accountId
    case availableUtxo of
        Left e -> return (Left $ NewTransactionUnknownAccount e)
        Right utxo -> do
            let mTx = prepareMTx config utxo changeaddr toSigner outputs aouts cmds mRemarks 
            let tx = case mTx of
                    Left e -> throw e
                    Right mtx -> mtx

            let inputs = map toMetaInputs $ snd tx
            let txAux = fst tx
            let txId = hash . taTx $ txAux
            let spentInputCoins = paymentAmount (_txAccountInputs $ taTx txAux) (toaOut . snd <$> inputs)

            txMetaCreatedAt_  <- liftIO $ Node.getCreationTimestamp (walletPassive ^. walletNode)
            partialMeta <- liftIO $ createNewMeta accountId txId txMetaCreatedAt_ inputs (_txUtxoOutputs . taTx $ txAux) [] (map toResolvedTxAccountOut aouts) cmds True spentInputCoins
            return $ Right (txAux, partialMeta, utxo)       
   
toTxOuts :: [(Address, Coin, Maybe SlotId)] 
         -> [(Address, GoldCoin)]
         -> [(Address, GoldDollar)]
         -> [TxOutAux]
toTxOuts s g d = map toTxOut s 
              <> map toTxOutGold g
              <> map toTxOutDollar d

toTxOut :: (Address, Coin, Maybe SlotId) -> TxOutAux
toTxOut (a, c, l) = TxOutAux (TxOutSeal a c l)

toTxOutGold :: (Address, GoldCoin) -> TxOutAux
toTxOutGold (a, c) = TxOutAux (TxOutGold a c)

toTxOutDollar :: (Address, GoldDollar) -> TxOutAux
toTxOutDollar (a, c) = TxOutAux (TxOutDollar a c)

toMetaInputs :: (TxOut, TxIn) -> (TxIn, TxOutAux)
toMetaInputs (txOut, txIn) = 
    let txOutAux = TxOutAux txOut
    in (txIn, txOutAux)

-- | This is called when we create a new Pending Transaction.
-- This actually returns a function because we don`t know yet our outputs.
createNewMeta :: HdAccountId -> TxId -> Core.Timestamp -> [(TxIn, TxOutAux)] -> [TxOut] -> 
                 [(Core.Address, Nonce, Core.CoinGroup)] -> [(Core.Address, Core.CoinGroup)] -> 
                 [Core.Cmd] -> Bool -> Core.CoinGroup -> IO PartialTxMeta
createNewMeta hdId txId time inp out ainpus aoutputs cmds allInOurs spentInputsCoins = do
    -- this partially applied function indicates the lack of all TxMeta at this stage.
    return $ metaForNewTx time hdId txId inp out ainpus aoutputs cmds allInOurs spentInputsCoins

metaForNewTx  :: Core.Timestamp -> HdAccountId -> TxId -> [(TxIn, TxOutAux)] -> [TxOut] -> 
                 [(Core.Address, Nonce, Core.CoinGroup)] -> [(Core.Address, Core.CoinGroup)] -> 
                 [Core.Cmd] -> Bool -> Core.CoinGroup -> Bool -> Core.CoinGroup -> TxMeta
metaForNewTx time accountId txId inputs outputs ainpus aoutputs cmds allInpOurs spentInputsCoins allOutOurs gainedOutputsCoins =
    TxMeta {
          _txMetaId = txId
        , _txMetaAmount = absCoinGroup spentInputsCoins gainedOutputsCoins
        , _txMetaInputs = inputsForMeta
        , _txMetaOutputs = outputsForMeta
        , _txMetaGoldInputs = goldInputsForMeta
        , _txMetaGoldOutputs = goldOutputsForMeta
        , _txMetaDollarInputs = dollarInputsForMeta
        , _txMetaDollarOutputs = dollarOutputsForMeta
        , _txMetaAccountInputs = ainpus
        , _txMetaAccountOutputs = aoutputs
        , _txMetaCmds = cmds
        , _txMetaCreationAt = time
        , _txMetaIsLocal = allInpOurs && allOutOurs
        , _txMetaIsOutgoing = gainedOutputsCoins < spentInputsCoins -- it`s outgoing if our inputs spent are more than the new utxo.
        , _txMetaWalletId = accountId ^. hdAccountIdParent
        , _txMetaAccountIx = getHdAccountIx $ accountId ^. hdAccountIdIx
    }
  where
    inputsForMeta        = toInput inputs
    goldInputsForMeta    = toGoldInput inputs
    dollarInputsForMeta  = toDollarInput inputs
    outputsForMeta       = toOutPut outputs
    goldOutputsForMeta   = toGoldOutPut outputs
    dollarOutputsForMeta = toDollarOutPut outputs
    
    toInput :: [(TxIn, TxOutAux)] -> [(TxId, Word32, Core.Address, Core.Coin, Maybe Core.SlotId)]
    toInput []                     = []
    toInput ((txin, txOutAux):ios) = case (txin, txOutAux) of
        ((TxInUtxo txid index), TxOutAux (TxOutSeal addr coin lock)) 
                               -> (txid, index, addr, coin, lock):(toInput ios)
        ((TxInUtxo _ _), _)    -> toInput ios
        ((TxInUnknown _ _), _) -> error "Tried to create TxMeta with unknown input"
    
    toGoldInput :: [(TxIn, TxOutAux)] -> [(TxId, Word32, Core.Address, Core.GoldCoin)]
    toGoldInput []                     = []
    toGoldInput ((txin, txOutAux):ios) = case (txin, txOutAux) of
        ((TxInUtxo txid index), TxOutAux (TxOutGold addr coin)) 
                               -> (txid, index, addr, coin):(toGoldInput ios)
        ((TxInUtxo _ _), _)    -> toGoldInput ios
        ((TxInUnknown _ _), _) -> error "Tried to create TxMeta with unknown input"
    
    toDollarInput :: [(TxIn, TxOutAux)] -> [(TxId, Word32, Core.Address, Core.GoldDollar)]
    toDollarInput []                     = []
    toDollarInput ((txin, txOutAux):ios) = case (txin, txOutAux) of
        ((TxInUtxo txid index), TxOutAux (TxOutDollar addr coin)) 
                               -> (txid, index, addr, coin):(toDollarInput ios)
        ((TxInUtxo _ _), _)    -> toDollarInput ios
        ((TxInUnknown _ _), _) -> error "Tried to create TxMeta with unknown input"

    toOutPut :: [TxOut] -> [(Core.Address, Core.Coin, Maybe Core.SlotId)]
    toOutPut ((TxOutSeal addr coin lock): txouts) = (addr, coin, lock):(toOutPut txouts)
    toOutPut (_: txouts)                          = toOutPut txouts
    toOutPut []                                   = []

    toGoldOutPut :: [TxOut] -> [(Core.Address, Core.GoldCoin)]
    toGoldOutPut ((TxOutGold addr coin): txouts) = (addr, coin):(toGoldOutPut txouts)
    toGoldOutPut (_: txouts)                     = toGoldOutPut txouts
    toGoldOutPut []                              = []

    toDollarOutPut :: [TxOut] -> [(Core.Address, Core.GoldDollar)]
    toDollarOutPut ((TxOutDollar addr coin): txouts) = (addr, coin):(toDollarOutPut txouts)
    toDollarOutPut (_: txouts)                       = toDollarOutPut txouts
    toDollarOutPut []                                = []


-- | Different wraper for @metaForNewTx@ mainly for testing Only NewPending Transactions.
toMeta :: Core.Timestamp -> HdAccountId -> RawResolvedTx -> PartialTxMeta
toMeta time accountId UnsafeRawResolvedTx{..} allOutOurs outCoin =
    let allInpOurs = True
        txId = hash . taTx $ rawResolvedTx
        txIn = _txUtxoInputs $ taTx rawResolvedTx
        inputsRes = rawResolvedTxInputs
        spentInputCoins = paymentAmount (_txAccountInputs $ taTx rawResolvedTx) (map toaOut inputsRes)
        inputs = zip txIn inputsRes
        txOut = _txUtxoOutputs $ taTx rawResolvedTx
        ainps = map toResolvedTxAccountIn $ _txAccountInputs $ taTx rawResolvedTx
        aouts = map toResolvedTxAccountOut $ _txAccountOutputs $ taTx rawResolvedTx
        cmds = _txCmds $ taTx rawResolvedTx
    in metaForNewTx time accountId txId inputs txOut ainps aouts cmds allInpOurs spentInputCoins allOutOurs outCoin

-- | Special monad used to process the payments, which randomness is derived
-- from a fixed seed obtained from hashing the payees. This guarantees that
-- when we estimate the fees and later create a transaction, the coin selection
-- will always yield the same value, making the process externally-predicatable.
newtype PayMonad a = PayMonad {
      buildPayment :: ReaderT Env IO a
    } deriving ( Functor , Applicative , Monad , MonadIO, MonadReader Env)

-- | This 'Env' datatype is necessary to convince GHC that indeed we have
-- a 'MonadReader' instance defined on 'GenIO' for the 'PayMonad'.
newtype Env = Env { getEnv :: System.Random.MWC.GenIO }

-- | \"Invalid\" 'MonadRandom' instance for 'PayMonad' which generates
-- randomness using the hash of 'NonEmpty (Address, Coin)' as fixed seed,
-- plus an internal counter used to shift the bits of such hash.
-- This ensures that the coin selection algorithm runs in a random environment
-- which is yet deterministically reproduceable by feeding the same set of
-- payees.
instance MonadRandom PayMonad where
    getRandomBytes len = do
        gen <- asks getEnv
        randomBytes <- liftIO (System.Random.MWC.asGenIO (flip System.Random.MWC.uniformVector len) gen)
        return $ ByteArray.convert (B.pack $ V.toList randomBytes)

{-------------------------------------------------------------------------------
  Estimating fees
-------------------------------------------------------------------------------}

data EstimateFeesError = EstFeesTxCreationFailed NewTransactionError

instance Buildable EstimateFeesError where
    build (EstFeesTxCreationFailed newTxErr) =
        bprint ("EstFeesTxCreationFailed " % build) newTxErr

instance Arbitrary EstimateFeesError where
    arbitrary = EstFeesTxCreationFailed <$> arbitrary

estimateFees :: ActiveWallet
             -> CoinSelectionOptions
             -- ^ The options describing how to tune the coin selection.
             -> HdAccountId
             -- ^ The source HD Account from where the payment should originate
             -> [(Address, Coin, Maybe SlotId)] 
             -> [(Address, GoldCoin)]
             -> [(Address, GoldDollar)]
             -- ^ The payees
             -> [AccountOut]
             -> [Core.Cmd]

             -> IO (Either EstimateFeesError Coin)
estimateFees activeWallet@ActiveWallet{..} options accountId payees payeesgold payeesdollar aouts cmds = do
    res <- newUnsignedTransaction activeWallet options accountId payees payeesgold payeesdollar aouts cmds
    case res of
         Left e  -> return . Left . EstFeesTxCreationFailed $ e
         Right (_db, _tx, fees, _originalUtxo) -> do
             -- sanity check of fees is done.
             return $ Right fees

-- | Calculate the fee as the difference between inputs and outputs. The
-- final 'sumOfOutputs' must be augmented by the change, which we have
-- available in the 'UnsignedTx' as a '[Coin]'.
--
-- NOTE(adn) In case of 'SenderPaysFee' is practice there might be a slightly
-- increase of the projected fee in the case we are forced to pick "yet another input"
-- to be able to pay the fee, which would, in turn, also increase the fee due to
-- the extra input being picked.
computeFeesOfUnsignedTx :: UnsignedTx -> Coin
computeFeesOfUnsignedTx unsginedTx =
    sumOfInputs unsginedTx
        `unsafeSubCoin`
                    (repeatedly unsafeAddCoin (unsignedTxChange unsginedTx)
                                                   (sumOfOutputs unsginedTx))
    where
        -- Tribute to @edsko
        repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
        repeatedly = flip . foldl' . flip

        -- Unlike a block, a /single transaction/ cannot have inputs that sum to
        -- more than maxCoinVal
        sumOfInputs :: UnsignedTx -> Coin
        sumOfInputs tx =
            let inputs = fmap (toaOut . snd) . unsignedTxInputs $ tx
                accountIns = unsignedTxAccountIns tx
            in Core.cgSeal (paymentAmount accountIns inputs)

        sumOfOutputs :: UnsignedTx -> Coin
        sumOfOutputs tx =
            let outputs = map toaOut $ unsignedTxOutputs tx
                accountOuts = flip map (unsignedTxAccountOuts tx) $ 
                  \DepositOutput {..} -> RedeemInput doAccount 0 doValue
                usum    = Core.cgSeal $ paymentAmount accountOuts outputs
                aouts   = map Core.cgSeal $ map doValue $ unsignedTxAccountOuts tx
                accsum  = sumTotals aouts
            in unsafeAddCoin usum accsum
        sumTotals :: [Core.Coin] -> Core.Coin
        sumTotals = foldl' unsafeAddCoin (mkCoin 0)
-- | Errors during transaction signing
--
-- NOTE: Under normal circumstances these should /never/ be thrown. If they
-- do, it most likely indicates a bug.
data SignTransactionError =
    SignTransactionMissingKey Address
  | SignTransactionErrorUnknownAddress Address
  | SignTransactionErrorNotOwned Address

instance Buildable SignTransactionError where
    build (SignTransactionMissingKey addr) =
        bprint ("SignTransactionMissingKey " % build) addr
    build (SignTransactionErrorUnknownAddress addr) =
        bprint ("SignTransactionErrorUnknownAddress " % build) addr
    build (SignTransactionErrorNotOwned addr) =
        bprint ("SignTransactionErrorNotOwned " % build) addr

-- in order to be able to generate an Arbitrary address we'd need to use
-- the cardano-sl-core test package
instance Arbitrary SignTransactionError where
    arbitrary = oneof
        [ SignTransactionMissingKey <$> arbitrary
        , SignTransactionErrorUnknownAddress <$> arbitrary
        , SignTransactionErrorNotOwned <$> arbitrary
        ]

mkSigner :: NetworkMagic
         -> PassPhrase
         -> Maybe EncryptedSecretKey
         -> DB
         -> Address
         -> Either SignTransactionError SafeSigner
mkSigner _ _ Nothing _ addr = Left (SignTransactionMissingKey addr)
mkSigner nm spendingPassword (Just esk) snapshot addr =
    case Getters.lookupCardanoAddress snapshot addr of
        Left _ -> Left (SignTransactionErrorUnknownAddress addr)
        Right hdAddr ->
            let addressIndex = hdAddr ^. HD.hdAddressId
                                       . HD.hdAddressIdIx
                                       . to HD.getHdAddressIx
                accountIndex = hdAddr ^. HD.hdAddressId
                                       . HD.hdAddressIdParent
                                       . HD.hdAccountIdIx
                                       . to HD.getHdAccountIx
                mAddressPayload = hdAddr ^. HD.hdAddressAddress
                                          . fromDb
                                          . to Core.addrAttributes
                                          . to attrData
                                          . to Core.aaPkDerivationPath
                res = case mAddressPayload of
                    -- If there is some payload we expect this payload to be addressIx and accountIx
                    -- used for old address scheme so we continue with using
                    -- old HD address derivation scheme: simple bip32 with ed25519 v0
                    Just _ -> Core.deriveLvl2KeyPair nm
                                    (Core.IsBootstrapEraAddr True)
                                    (ShouldCheckPassphrase False)
                                    spendingPassword
                                    esk
                                    accountIndex
                                    addressIndex
                    -- If there is no payload we assume it is a new address scheme (which doesn't have payload)
                    -- New HD address derivation scheme: bip44 with ed25519 v1
                    Nothing -> first (Core.makePubKeyAddressBoot nm) <$>
                        deriveAddressKeyPair
                            spendingPassword
                            esk
                            accountIndex
                            ExternalChain
                            addressIndex

            -- eks address fix - we need to use the esk as returned
            -- from Core.deriveLvl2KeyPair rather than rely on the
            -- one from encrypted secret key delivered to mkSigner
            in case res of
                 Just (a, eskAddr) | a == addr ->
                     Right (SafeSigner eskAddr spendingPassword)
                 _otherwise              ->
                     Left (SignTransactionErrorNotOwned addr)

-- | An estimate of the Tx fees in Cardano based on a sensible number of defaults.
cardanoFee :: TxFeePolicy -> Int -> NonEmpty Coin -> Coin
cardanoFee (TxFeePolicyTxSizeLinear policy) inputs outputs =
    mkCoin $
      estimateCardanoFee policy inputs (toList $ fmap Core.getCoin outputs)
cardanoFee TxFeePolicyUnknown{} _ _ =
    error "cardanoFee: unknown policy"

-- | Generates the list of change outputs from a list of change coins.
genChangeOuts :: MonadIO m
              => [Coin]
              -> HD.HdAccountId
              -> PassPhrase
              -> PassiveWallet
              -> ExceptT Kernel.CreateAddressError m [TxOutAux]
genChangeOuts changeCoins srcAccountId spendingPassword walletPassive =
    forM changeCoins $ \change -> do
        changeAddr <- genChangeAddr
        return TxOutAux {
            toaOut = TxOutSeal
                { txOutAddress  = changeAddr
                , txOutSeal     = change
                , txOutLockTime = Nothing
                }
        }
  where
    genChangeAddr :: MonadIO m
                  => ExceptT Kernel.CreateAddressError m Address
    genChangeAddr = ExceptT $ liftIO $
        Kernel.createAddress spendingPassword
                             srcAccountId
                             walletPassive

{-------------------------------------------------------------------------------
  Building transactions
-------------------------------------------------------------------------------}

-- | Our notion of @unsigned transaction@. Unfortunately we cannot reuse
-- directly the 'Tx' from @Core@ as that discards the information about
-- "ownership" of inputs, which is instead required when dealing with the
-- Core Txp.Util API.
data UnsignedTx = UnsignedTx {
      unsignedTxInputs      :: ![(Core.TxIn, Core.TxOutAux)]
    , unsignedTxOutputs     :: ![Core.TxOutAux]
    , unsignedTxAccountIns  :: ![AccountIn]
    , unsignedTxAccountOuts :: ![AccountOut]
    , _unsignedTxCmds        :: ![Core.Cmd]
    , unsignedTxAttributes  :: !Core.TxAttributes
    , unsignedTxChange      :: ![Core.Coin]
}

-- | Build a transaction

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: Monad m
        => ProtocolMagic
        -> (forall a. [a] -> m [a])
        -- ^ Shuffle function
        -> (Core.Address -> Either e SafeSigner)
        -- ^ Signer for each input of the transaction
        -> TxAttributes
        -> [(Core.TxIn, Core.TxOutAux)]
        -- ^ Selected inputs
        -> [Core.TxOutAux]
        -- ^ Selected outputs
        -> [AccountOut]
        -> [Core.Cmd]
        -- ^ Selected account
        -> [Core.TxOutAux]
        -- ^ Change outputs
        -> m (Either e Core.TxAux)
mkStdTx pm shuf hdwSigners txAttrs inps outs aouts cmds change = do
    allOuts <- shuf $ outs <> change
    return $ CTxp.makeMPubKeyTxAddrs pm hdwSigners txAttrs aouts cmds (map repack inps) allOuts
    where
         -- | Repacks a utxo-derived tuple into a format suitable for
         -- 'TxOwnedInputs'.
        repack :: (Core.TxIn, Core.TxOutAux) -> (Core.TxOut, Core.TxIn)
        repack (txIn, aux) = (Core.toaOut aux, txIn)

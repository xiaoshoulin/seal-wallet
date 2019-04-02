{-# LANGUAGE LambdaCase #-}

module Seal.Wallet.WalletLayer.Kernel.Transactions (
      getTransactions
    , toTransaction
) where

import           Universum

import           Control.Monad.Except
import           Formatting (build, sformat)
import           GHC.TypeLits (symbolVal)

import           Seal.Core (Address, Coin, SlotCount, SlotId,
                     GoldCoin, GoldDollar, CoinGroup (..),
                     flattenSlotId, getBlockCount)
import           Seal.Chain.Txp (Nonce)
import           Seal.Wallet.API.V1.Info (unV1)
import           Seal.Util.Wlog (Severity (..))

import           Seal.Wallet.API.Indices
import           Seal.Wallet.API.Request
import qualified Seal.Wallet.API.Request.Filter as F
import           Seal.Wallet.API.Request.Pagination
import qualified Seal.Wallet.API.Request.Sort as S
import           Seal.Wallet.API.Response
import qualified Seal.Wallet.API.V1.Types as V1
import qualified Seal.Wallet.Kernel.DB.HdRootId as HD
import qualified Seal.Wallet.Kernel.DB.HdWallet as HD
import           Seal.Wallet.Kernel.DB.TxMeta (TxMeta (..))
import qualified Seal.Wallet.Kernel.DB.TxMeta as TxMeta
import qualified Seal.Wallet.Kernel.Internal as Kernel
import qualified Seal.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Seal.Wallet.Kernel.Read as Kernel
import           Seal.Wallet.WalletLayer (GetTxError (..))
import           UTxO.Util (exceptT)

getTransactions :: MonadIO m
                => Kernel.PassiveWallet
                -> Maybe V1.WalletId
                -> Maybe V1.AccountIndex
                -> Maybe V1.WalAddress
                -> RequestParams
                -> FilterOperations '[V1.WalletTxId, V1.WalletTimestamp] V1.Transaction
                -> SortOperations V1.Transaction
                -> m (Either GetTxError (APIResponse [V1.Transaction]))
getTransactions wallet mbWalletId mbAccountIndex mbAddress params fop sop = liftIO $ runExceptT $ do
    let PaginationParams{..}  = rpPaginationParams params
    let PerPage pp = ppPerPage
    let Page cp = ppPage
    (txs, total) <- go cp pp ([], Nothing)
    return $ respond params txs total
  where
    -- NOTE: See cardano-wallet#141
    --
    -- We may end up with some inconsistent metadata in the store. When fetching
    -- them all, instead of failing with a non very helpful 'WalletNotfound' or
    -- 'AccountNotFound' error because one or more metadata in the list contains
    -- unknown ids, we simply discard them from what we fetched and we fetch
    -- another batch up until we have enough (== pp).
    go cp pp (acc, total)
        | length acc >= pp =
            return $ (take pp acc, total)
        | otherwise = do
            accountFops <- castAccountFiltering mbWalletId mbAccountIndex
            mbSorting <- castSorting sop
            (metas, mbTotalEntries) <- liftIO $ TxMeta.getTxMetas
                (wallet ^. Kernel.walletMeta)
                (TxMeta.Offset . fromIntegral $ (cp - 1) * pp)
                (TxMeta.Limit . fromIntegral $ pp)
                accountFops
                (V1.unWalAddress <$> mbAddress)
                (castFiltering $ mapIx unV1 <$> F.findMatchingFilterOp fop)
                (castFiltering $ mapIx unV1 <$> F.findMatchingFilterOp fop)
                mbSorting
            if null metas then
                -- A bit artificial, but we force the termination and make sure
                -- in the meantime that the algorithm only exits by one and only
                -- one branch.
                go cp (min pp $ length acc) (acc, total <|> mbTotalEntries)
            else do
                txs <- catMaybes <$> forM metas (\meta -> do
                    toTransaction wallet meta >>= \case
                        Left e -> do
                            let warn = lift . ((wallet ^. Kernel.walletLogMessage) Warning)
                            warn $ "Inconsistent entry in the metadata store: " <> sformat build e
                            return Nothing

                        Right tx ->
                            return (Just tx)
                    )
                go (cp + 1) pp (acc ++ txs, total <|> mbTotalEntries)


toTransaction :: MonadIO m
              => Kernel.PassiveWallet
              -> TxMeta
              -> m (Either HD.UnknownHdAccount V1.Transaction)
toTransaction wallet meta = liftIO $ do
    db <- liftIO $ Kernel.getWalletSnapshot wallet
    sc <- liftIO $ Node.getSlotCount (wallet ^. Kernel.walletNode)
    currentSlot <- Node.getTipSlotId (wallet ^. Kernel.walletNode)
    return $ runExcept $ metaToTx db sc currentSlot meta

-- | Type Casting for Account filtering from V1 to MetaData Types.
castAccountFiltering :: Monad m => Maybe V1.WalletId -> Maybe V1.AccountIndex -> ExceptT GetTxError m TxMeta.AccountFops
castAccountFiltering mbWalletId mbAccountIndex =
    case (mbWalletId, mbAccountIndex) of
        (Nothing, Nothing) -> return TxMeta.Everything
        (Nothing, Just _)  -> throwError GetTxMissingWalletIdError
        -- AccountIndex doesn`t uniquely identify an Account, so we shouldn`t continue without a WalletId.
        (Just (V1.WalletId wId), _) ->
            case HD.decodeHdRootId wId of
                Nothing     -> throwError $ GetTxAddressDecodingFailed wId
                Just rootId -> return $ TxMeta.AccountFops rootId (V1.getAccIndex <$> mbAccountIndex)

-- This function reads at most the head of the SortOperations and expects to find "created_at".
castSorting :: Monad m => S.SortOperations V1.Transaction -> ExceptT GetTxError m (Maybe TxMeta.Sorting)
castSorting S.NoSorts = return Nothing
castSorting (S.SortOp (sop :: S.SortOperation ix V1.Transaction) _) =
    case symbolVal (Proxy @(IndexToQueryParam V1.Transaction ix)) of
        "created_at" -> return $ Just $ TxMeta.Sorting TxMeta.SortByCreationAt (castSortingDirection sop)
        txt -> throwError $ GetTxInvalidSortingOperation txt

castSortingDirection :: S.SortOperation ix a -> TxMeta.SortDirection
castSortingDirection (S.SortByIndex srt _) = case srt of
    S.SortAscending  -> TxMeta.Ascending
    S.SortDescending -> TxMeta.Descending

castFiltering :: Maybe (F.FilterOperation ix V1.Transaction) -> TxMeta.FilterOperation ix
castFiltering mfop = case mfop of
    Nothing -> TxMeta.NoFilterOp
    Just fop -> case fop of
        (F.FilterByIndex q)         -> TxMeta.FilterByIndex q
        (F.FilterByPredicate prd q) -> TxMeta.FilterByPredicate (castFilterOrd prd) q
        (F.FilterByRange q w)       -> TxMeta.FilterByRange q w
        (F.FilterIn ls)             -> TxMeta.FilterIn ls

castFilterOrd :: F.FilterOrdering -> TxMeta.FilterOrdering
castFilterOrd pr = case pr of
    F.Equal            -> TxMeta.Equal
    F.GreaterThan      -> TxMeta.GreaterThan
    F.GreaterThanEqual -> TxMeta.GreaterThanEqual
    F.LesserThan       -> TxMeta.LesserThan
    F.LesserThanEqual  -> TxMeta.LesserThanEqual

metaToTx :: Monad m => Kernel.DB -> SlotCount -> SlotId -> TxMeta -> ExceptT HD.UnknownHdAccount m V1.Transaction
metaToTx db slotCount current TxMeta{..} = do
    mSlotwithState <- withExceptT identity $ exceptT $
                        Kernel.currentTxSlotId db _txMetaId hdAccountId
    isPending      <- withExceptT identity $ exceptT $
                        Kernel.currentTxIsPending db _txMetaId hdAccountId
    assuranceLevel <- withExceptT HD.embedUnknownHdRoot $ exceptT $
                        Kernel.rootAssuranceLevel db hdRootId
    let (status, confirmations) = buildDynamicTxMeta assuranceLevel slotCount mSlotwithState current isPending
    return V1.Transaction {
        txId = V1.WalletTxId _txMetaId,
        txConfirmations = fromIntegral confirmations,
        txAmount = V1.WalCoinGroup _txMetaAmount,
        txInputs = inputsToPayDistr <$> _txMetaInputs,
        txOutputs = outputsToPayDistr <$> _txMetaOutputs,
        txGoldInputs = goldInputsToPayDistr <$> _txMetaGoldInputs,
        txGoldOutputs = goldOutputsToPayDistr <$> _txMetaGoldOutputs,
        txDollarInputs = dollarInputsToPayDistr <$> _txMetaDollarInputs,
        txDollarOutputs = dollarOutputsToPayDistr <$> _txMetaDollarOutputs,
        txAccountInputs = accountInputsToPayDistr <$> _txMetaAccountInputs,
        txAccountOutputs = accountOutputsToPayDistr <$> _txMetaAccountOutputs,
        txCmds = V1.WalCmd <$> _txMetaCmds,
        txType = if _txMetaIsLocal then V1.LocalTransaction else V1.ForeignTransaction,
        txDirection = if _txMetaIsOutgoing then V1.OutgoingTransaction else V1.IncomingTransaction,
        txCreationTime = V1.WalletTimestamp _txMetaCreationAt,
        txStatus = status
    }
        where
            hdRootId    = _txMetaWalletId
            hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx _txMetaAccountIx)

            inputsToPayDistr :: (a , b, Address, Coin, Maybe SlotId) -> V1.PaymentDistribution
            inputsToPayDistr (_, _, addr, c, s) = V1.PaymentDistribution (V1.WalAddress addr) (V1.WalletCoin c) (V1.WalSlotId s)

            outputsToPayDistr :: (Address, Coin, Maybe SlotId) -> V1.PaymentDistribution
            outputsToPayDistr (addr, c, s) = V1.PaymentDistribution (V1.WalAddress addr) (V1.WalletCoin c) (V1.WalSlotId s)

            goldInputsToPayDistr :: (a , b, Address, GoldCoin) -> V1.GoldPaymentDistribution
            goldInputsToPayDistr (_, _, addr, c) = V1.GoldPaymentDistribution (V1.WalAddress addr) (V1.WalGoldCoin c)

            goldOutputsToPayDistr :: (Address, GoldCoin) -> V1.GoldPaymentDistribution
            goldOutputsToPayDistr (addr, c) = V1.GoldPaymentDistribution (V1.WalAddress addr) (V1.WalGoldCoin c)

            dollarInputsToPayDistr :: (a , b, Address, GoldDollar) -> V1.DollarPaymentDistribution
            dollarInputsToPayDistr (_, _, addr, c) = V1.DollarPaymentDistribution (V1.WalAddress addr) (V1.WalGoldDollar c)

            dollarOutputsToPayDistr :: (Address, GoldDollar) -> V1.DollarPaymentDistribution
            dollarOutputsToPayDistr (addr, c) = V1.DollarPaymentDistribution (V1.WalAddress addr) (V1.WalGoldDollar c)

            accountInputsToPayDistr :: (Address, Nonce, CoinGroup) -> V1.AccountInputDistribution
            accountInputsToPayDistr (addr, n, c) = V1.AccountInputDistribution (V1.WalAddress addr) (V1.WalNonce n) (V1.WalCoinGroup c)

            accountOutputsToPayDistr :: (Address, CoinGroup) -> V1.AccountOutputDistribution
            accountOutputsToPayDistr (addr, CoinGroup{..}) = V1.AccountOutputDistribution (V1.WalAddress addr) 
                                                                                          (V1.WalletCoin cgSeal)
                                                                                          (V1.WalGoldCoin cgGold) 
                                                                                          (V1.WalGoldDollar cgDollar)

buildDynamicTxMeta :: HD.AssuranceLevel -> SlotCount -> HD.CombinedWithAccountState (Maybe SlotId) -> SlotId -> Bool -> (V1.TransactionStatus, Word64)
buildDynamicTxMeta assuranceLevel slotCount mSlotwithState currentSlot isPending =
    let currentSlot' = flattenSlotId slotCount currentSlot
        goWithSlot confirmedIn =
            let confirmedIn'  = flattenSlotId slotCount confirmedIn
                confirmations = currentSlot' - confirmedIn'
            in case (confirmations < getBlockCount (HD.assuredBlockDepth assuranceLevel)) of
                True  -> (V1.InNewestBlocks, confirmations)
                False -> (V1.Persisted, confirmations)
    in case isPending of
        True  -> (V1.Applying, 0)
        False ->
            case mSlotwithState of
                HD.UpToDate Nothing     -> (V1.WontApply, 0)
                HD.Incomplete Nothing   -> (V1.Applying, 0)  -- during restoration, we report txs not found yet as Applying.
                HD.UpToDate (Just sl)   -> goWithSlot sl
                HD.Incomplete (Just sl) -> goWithSlot sl

-- | We don`t fitler in memory, so totalEntries is unknown, unless TxMeta Database counts them for us.
-- It is possible due to some error, to have length ls < Page.
-- This can happen when a Tx is found without Inputs.
respond :: RequestParams -> [a] -> Maybe Int -> (APIResponse [a])
respond RequestParams{..} ls mbTotalEntries =
    let totalEntries = fromMaybe 0 mbTotalEntries
        PaginationParams{..}  = rpPaginationParams
        perPage@(PerPage pp)  = ppPerPage
        currentPage           = ppPage
        totalPages            = max 1 $ ceiling (fromIntegral totalEntries / (fromIntegral pp :: Double))
        metadata              = PaginationMetadata {
                                metaTotalPages = totalPages
                                , metaPage = currentPage
                                , metaPerPage = perPage
                                , metaTotalEntries = totalEntries
                                }
    in  APIResponse {
        wrData = ls
      , wrStatus = SuccessStatus
      , wrMeta = Metadata metadata
      }

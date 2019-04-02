{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}

module Seal.Wallet.Kernel.Restore
    ( restoreWallet
    , restoreKnownWallet
    , continueRestoration
    , blundToResolvedBlock
    , mkPrefilter
    ) where

import           Universum

import           Control.Concurrent.Async (async, cancel)
import           Control.Lens (at, _Just)
import           Data.Acid (update)
import           Data.List (unzip3)
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Data.Traversable (for)
import           Formatting (bprint, build, formatToString, sformat, (%))
import qualified Formatting.Buildable

import qualified Prelude


import           Seal.Wallet.API.Types.UnitOfMeasure
import           Seal.Wallet.Kernel (walletLogMessage)
import qualified Seal.Wallet.Kernel as Kernel
import           Seal.Wallet.Kernel.DB.AcidState (ApplyHistoricalBlocks (..),
                     CreateHdWallet (..), ResetAllHdWalletAccounts (..),
                     RestorationComplete (..), RestoreHdWallet (..),
                     dbHdWallets)
import           Seal.Wallet.Kernel.DB.BlockContext
import qualified Seal.Wallet.Kernel.DB.HdRootId as HD
import qualified Seal.Wallet.Kernel.DB.HdWallet as HD
import           Seal.Wallet.Kernel.DB.HdWallet.Create (CreateHdRootError)
import           Seal.Wallet.Kernel.DB.InDb (fromDb)
import           Seal.Wallet.Kernel.DB.Resolved (ResolvedBlock,
                     resolvedToTxMetas)
import qualified Seal.Wallet.Kernel.DB.Spec.Pending as Pending
import qualified Seal.Wallet.Kernel.DB.Spec.Update as Spec
import           Seal.Wallet.Kernel.DB.TxMeta.Types
import           Seal.Wallet.Kernel.Internal (WalletRestorationInfo (..),
                     WalletRestorationProgress (..), addOrReplaceRestoration,
                     cancelRestoration, lookupRestorationInfo,
                     removeRestoration, restartRestoration, walletKeystore,
                     walletMeta, walletNode, walletProtocolMagic, wallets,
                     wrpCurrentSlot, wrpTargetSlot, wrpThroughput)
import qualified Seal.Wallet.Kernel.Keystore as Keystore
import           Seal.Wallet.Kernel.NodeStateAdaptor (Lock, LockContext (..),
                     NodeConstraints, NodeStateAdaptor, WithNodeState,
                     defaultGetSlotStart, filterUtxo, getCoreConfig,
                     getSecurityParameter, getSlotCount, mostRecentMainBlock,
                     withNodeState)
import           Seal.Wallet.Kernel.Prefiltering (PrefilteredBlock,
                     prefilterBlock, selectPrefixSeal, selectPrefixGold,
                     selectPrefixDollar, prefilterGoldUtxo, prefilterDollarUtxo,
                     prefilterUtxo)
import           Seal.Wallet.Kernel.Read (foreignPendingByAccount,
                     getWalletSnapshot)
import           Seal.Wallet.Kernel.Types (RawResolvedBlock (..),
                     fromRawResolvedBlock, rawResolvedBlock,
                     rawResolvedBlockInputs, rawResolvedContext, rawTimestamp)
import           Seal.Wallet.Kernel.Util.Core (utxoBalance, utxoGoldBalance, 
                     utxoDollarBalance)
import           Seal.Wallet.Kernel.Wallets (createWalletHdRnd)
import           Seal.Wallet.Core (mkCoin, mkGoldCoin, mkGoldDollar, 
                      unsafeIntegerToCoin, unsafeIntegerToGoldCoin,
                      unsafeIntegerToGoldDollar)

import           Seal.Chain.Block (Block, Blund, HeaderHash, Undo, mainBlockSlot,
                     undoTx)
import           Seal.Chain.Genesis (GenesisHash, configGenesisData,
                     configGenesisHash)
import qualified Seal.Chain.Genesis as Genesis (Config (..))
import           Seal.Chain.Txp (TxIn (..), TxOut (..), TxOutAux (..), Utxo,
                     genesisUtxo)
import           Seal.Core as Core (Address, BlockCount (..), Coin, SlotId,
                     flattenSlotId, getCurrentTimestamp, GoldCoin, GoldDollar)
import           Seal.Core.NetworkMagic (makeNetworkMagic)
import           Seal.Crypto (EncryptedSecretKey)
import           Seal.DB.Block (getFirstGenesisBlockHash, getUndo,
                     resolveForwardLink)
import           Seal.DB.Class (getBlock)
import           Seal.Util.Trace (Severity (Error))

-- | Restore a wallet
--
-- Scan the node's current UTXO set for any that belong to this wallet. Use them
-- to update the current checkpoint's UTXO set, and return the total 'Coin'
-- value of the UTXO belonging to this wallet. At the same time, kick off a
-- background thread that will asynchronously restore the wallet history.
--
-- Wallet initialization parameters match those of 'createWalletHdRnd'
--
-- NOTE: We pass in an optional fresh 'Address' which will be used to initialise
-- the companion 'HdAccount' this wallet will be created with. The reason why we
-- do this is that, if we were to use the 'PassPhrase' directly, it would have
-- been impossible for upstream code dealing with migrations to call this
-- function, as during migration time you don't have access to the users'
-- spending passwords.
--
-- During migration, instead, you can pick one of the @existing@ addresses
-- in the legacy wallet layer, and use it as input.
restoreWallet
    :: Kernel.PassiveWallet
    -> Bool
    -- ^ Did this wallet have a spending password set?
    -> Maybe Core.Address
    -- ^ An optional stock address to use for the companion 'HdAccount'.
    -> HD.WalletName
    -> HD.AssuranceLevel
    -> EncryptedSecretKey
    -> IO (Either CreateHdRootError (HD.HdRoot, Coin, GoldCoin, GoldDollar))
restoreWallet pw hasSpendingPassword defaultCardanoAddress name assurance esk
  = do
    coreConfig <- getCoreConfig (pw ^. walletNode)
    walletInitInfo <- withNodeState (pw ^. walletNode)
        $ getWalletInitInfo coreConfig creds
    case walletInitInfo of
        WalletCreate utxos -> do
            root <- createWalletHdRnd'
                $ \root defaultHdAccount defaultHdAddress -> do
                    let defUtxo = M.singleton
                            (HD.HdAccountBaseFO defaultHdAccount)
                            (mempty, maybeToList defaultHdAddress)
                    let utxos' = M.mapKeys HD.HdAccountBaseFO utxos
                    Left $ CreateHdWallet root (M.unionWith (<>) utxos' defUtxo)
            return $ fmap (, mkCoin 0, mkGoldCoin 0, mkGoldDollar 0) root
        WalletRestore utxos tgt -> do
            -- Create the wallet for restoration, deleting the wallet first if it
            -- already exists.
            mRoot <- createWalletHdRnd'
                $ \root defaultHdAccount defaultHdAddress -> Right
                $ RestoreHdWallet root defaultHdAccount defaultHdAddress tgt utxos
            for mRoot $ \root -> do
                -- Start the restoration task, from the genesis block up to
                -- @tgt@.
                beginRestoration pw prefilter root Nothing tgt (restart root)

                -- Return the wallet's current balance.
                let coins = unsafeIntegerToCoin
                          . utxoBalance
                          . M.unions
                          . M.elems
                          . fmap (\(cur, _gen, _addrs) -> cur)
                          $ utxos
                let goldcoins = unsafeIntegerToGoldCoin
                              . utxoGoldBalance
                              . M.unions
                              . M.elems
                              . fmap (\(cur, _gen, _addrs) -> cur)
                              $ utxos
                let dollarcoins = unsafeIntegerToGoldDollar
                                . utxoDollarBalance
                                . M.unions
                                . M.elems
                                . fmap (\(cur, _gen, _addrs) -> cur)
                                $ utxos
                return (root, coins, goldcoins, dollarcoins)

  where
    createWalletHdRnd' =
        createWalletHdRnd
            pw hasSpendingPassword defaultCardanoAddress name assurance esk
    nm = makeNetworkMagic (pw ^. walletProtocolMagic)
    creds = (HD.eskToHdRootId nm esk, esk)

    prefilter :: Blund -> IO (PrefilteredBlock, [TxMeta])
    prefilter = mkPrefilter pw creds

    restart :: HD.HdRoot -> IO ()
    restart root = do
        coreConfig <- getCoreConfig (pw ^. walletNode)
        walletInitInfo <- withNodeState (pw ^. walletNode)
            $ getWalletInitInfo coreConfig creds
        case walletInitInfo of
            WalletCreate _utxos ->
                -- This can only happen if the node has no main blocks,
                -- which is quite unlikely. For now, silently fail.
                return ()
            WalletRestore utxos tgt -> do
                update (pw ^. wallets)
                    $ ResetAllHdWalletAccounts (root ^. HD.hdRootId) tgt utxos
                beginRestoration pw prefilter root Nothing tgt (restart root)

-- | Synchronously restore the wallet balance, and begin to asynchronously
-- reconstruct the wallet's history.
mkPrefilter
    :: Kernel.PassiveWallet
    -> (HD.HdRootId, EncryptedSecretKey)
    -> Blund
    -> IO (PrefilteredBlock, [TxMeta])
mkPrefilter wallet (rootId,esk) blund = do
    foreigns <- fmap Pending.txIns . foreignPendingByAccount <$> getWalletSnapshot wallet
    blundToResolvedBlock (wallet ^. Kernel.walletNode) blund >>= \case
        Nothing -> return (mempty, [])
        Just rb -> flip evalStateT (M.singleton rootId esk) $ do
            pb <- state $ prefilterBlock foreigns rb
            metas <- state (resolvedToTxMetas rb) >>= \case
                Left e  -> throwM e
                Right x -> return x
            return (pb, metas)

-- | Begin a restoration for a wallet that is already known. This is used
-- to put an existing wallet back into a restoration state when something has
-- gone wrong.
restoreKnownWallet
    :: Kernel.PassiveWallet
    -> HD.HdRootId
    -> IO ()
restoreKnownWallet pw rootId = do
    let nm  = makeNetworkMagic (pw ^. walletProtocolMagic)
    lookupRestorationInfo pw rootId >>= \case
        -- Restart a pre-existing restoration
        Just wri -> do
            cancelRestoration  wri
            restartRestoration wri

        -- Start a new restoration of a seemingly up-to-date wallet.
        Nothing -> do
            mesk <- Keystore.lookup nm rootId (pw ^. walletKeystore)
            whenJust mesk $ \esk -> do
                let prefilter = mkPrefilter pw (rootId, esk)
                coreConfig <- getCoreConfig (pw ^. walletNode)
                db <- getWalletSnapshot pw
                let mroot = db ^. dbHdWallets . HD.hdWalletsRoots . at rootId
                -- TODO (@mn): the 'Nothing' case should not happen. add an
                -- assertion along those lines?
                whenJust mroot $ \root -> do
                    let restart = do
                            cmd <- withNodeState
                                (pw ^. walletNode)
                                (getWalletInitInfo coreConfig (rootId, esk))
                            case cmd of
                                WalletCreate  _utxos    ->
                                    -- This can only happen if the node has no
                                    -- main blocks, which is quite unlikely. For
                                    -- now, silently fail.
                                    return ()
                                WalletRestore utxos tgt -> do
                                    update
                                        (pw ^. wallets)
                                        $ ResetAllHdWalletAccounts rootId tgt utxos
                                    beginRestoration pw prefilter root Nothing tgt restart
                    restart

-- | Take a wallet that is in an incomplete state but not restoring, and
-- start up a restoration task for it. This is used to bring up restoration
-- tasks for any accounts in incomplete states when the wallet starts up.
continueRestoration
    :: Kernel.PassiveWallet
    -> HD.HdRoot
    -> Maybe BlockContext
    -> BlockContext
    -> IO ()
continueRestoration pw root cur tgt = do
    let nm  = makeNetworkMagic (pw ^. walletProtocolMagic)
        wId = root ^. HD.hdRootId
    Keystore.lookup nm wId (pw ^. walletKeystore) >>= \case
        Nothing  ->
            -- TODO (@mn): raise an error, trying to continue
            -- restoration of an unknown wallet
            return ()
        Just esk -> do
            let creds = (root ^. HD.hdRootId, esk)
            let prefilter = mkPrefilter pw creds
            let restart   = do
                    coreConfig <- getCoreConfig (pw ^. walletNode)
                    wii <- withNodeState (pw ^. walletNode)
                                        (getWalletInitInfo coreConfig creds)
                    case wii of
                        WalletCreate  _utxos    ->
                            -- This can only happen if the node has no main
                            -- blocks, which is quite unlikely. For now,
                            -- silently fail.
                            return ()
                        WalletRestore utxos tgt' -> do
                            update (pw ^. wallets)
                                $ ResetAllHdWalletAccounts
                                (root ^. HD.hdRootId) tgt' utxos
                            beginRestoration pw prefilter root Nothing tgt' restart
            beginRestoration pw prefilter root cur tgt restart

-- | Register and start up a background restoration task.
beginRestoration
    :: Kernel.PassiveWallet
   -> (Blund -> IO (PrefilteredBlock, [TxMeta]))
   -> HD.HdRoot
   -> Maybe BlockContext
   -- ^ The block to start from, or Nothing to start from the genesis block.
   -> BlockContext
   -> IO ()
   -> IO ()
beginRestoration pw prefilter root cur tgt restart = do
    let tgtTip  = tgt ^. bcHash   . fromDb
        tgtSlot = tgt ^. bcSlotId . fromDb

    -- Set the wallet's restoration information
    slotCount <- getSlotCount (pw ^. walletNode)
    let toSlotId = flattenSlotId slotCount
    progress <- newIORef
        $ WalletRestorationProgress
        { _wrpCurrentSlot = maybe 0 (toSlotId . view (bcSlotId . fromDb)) cur
        , _wrpTargetSlot  = toSlotId tgtSlot
        , _wrpThroughput  = MeasuredIn 0
        }

    -- Begin restoring the wallet history in the background.
    restoreTask <- async $ do
        -- We are starting this async /from/ a thread that runs in response
        -- to a REST request. Linking the async to that REST request thread
        -- is pointless, because that thread will probably be long gone if
        -- an exception ever happens in the restoration worker. Therefore
        -- we just log any errors.
        let rootId = root ^. HD.hdRootId
            mhash = cur ^? _Just . bcHash . fromDb
            tipslot = (tgtTip, tgtSlot)
        restoreWalletHistoryAsync pw rootId prefilter progress mhash tipslot
            `catch` \(e :: SomeException) ->
                (pw ^. walletLogMessage) Error $
                    sformat
                        ( "Exception during restoration of wallet"
                        % build
                        % " when starting from " % build
                        % " with target " % build
                        % ". Exception: " % build
                        )
                        (root ^. HD.hdRootId)
                        (maybe "genesis" pretty cur)
                        (pretty tgt)
                        e

    theTask <- newMVar restoreTask
    addOrReplaceRestoration pw (root ^. HD.hdRootId) $ WalletRestorationInfo
        { _wriProgress = readIORef progress
        , _wriCancel   = readMVar theTask >>= cancel
        , _wriRestart  = restart
        }

-- | Information we need to start the restoration process
data WalletInitInfo =
    -- | Create the wallet, without actually restoring
    --
   -- This is used only when the chain has no main blocks yet. We record
    -- the only the genesis UTxO for the wallet, and any addresses we found.
    WalletCreate
      (Map HD.HdAccountId (Utxo, [HD.HdAddress]))

    -- | Restore the wallet
    --
    -- We record the current and genesis UTxO, as well as some information
    -- about the most recent main block on the chain.
  | WalletRestore
      (Map HD.HdAccountId (Utxo, Utxo, [HD.HdAddress]))
      BlockContext

-- | Query the underlying node for the info we need to restore a wallet
--
-- We return the current and genesis UTxO for this wallet, as well some
-- information about the tip of the blockchain (provided the blockchain
-- isn't empty).
getWalletInitInfo
    :: NodeConstraints
    => Genesis.Config
    -> (HD.HdRootId, EncryptedSecretKey)
    -> Lock (WithNodeState IO)
    -> WithNodeState IO WalletInitInfo
getWalletInitInfo coreConfig (rootId,esk) lock = do
    -- Find all of the current UTXO that this wallet owns.
    -- We lock the node state to be sure the tip header and the UTxO match
    (tipHeader, curUtxo) <-
        fmap (second (fmap toPrefilteredUtxo . mergeUtxos)) $
            lock NotYetLocked $ \tip -> (tip, ) <$> filterUtxo isOurs

    -- Find genesis UTxO for this wallet
    let genSealUtxo :: Map HD.HdAccountId (Utxo, [HD.HdAddress])
        genSealUtxo = M.unionsWith (<>) $ flip evalState (M.singleton rootId esk) $ do
            fmap (fmap byAccount . M.toList) $ state $
                prefilterUtxo (selectPrefixSeal $ M.assocs $ genesisUtxo $ configGenesisData coreConfig)
              where
                byAccount (txin, (hdAddr, coin, lockSlot)) =
                    M.singleton accId (utxo, [hdAddr])
                  where
                    accId = hdAddr ^. HD.hdAddressId . HD.hdAddressIdParent
                    addr = hdAddr ^. HD.hdAddressAddress . fromDb
                    utxo = M.singleton txin (TxOutAux $ TxOutSeal addr coin lockSlot)
    let genGoldUtxo :: Map HD.HdAccountId (Utxo, [HD.HdAddress])
        genGoldUtxo = M.unionsWith (<>) $ flip evalState (M.singleton rootId esk) $ do
            fmap (fmap byAccount . M.toList) $ state $
                prefilterGoldUtxo (selectPrefixGold $ M.assocs $ genesisUtxo $ configGenesisData coreConfig)
              where
                byAccount (txin, (hdAddr, coin)) =
                    M.singleton accId (utxo, [hdAddr])
                  where
                    accId = hdAddr ^. HD.hdAddressId . HD.hdAddressIdParent
                    addr = hdAddr ^. HD.hdAddressAddress . fromDb
                    utxo = M.singleton txin (TxOutAux $ TxOutGold addr coin)
    let genDollarUtxo :: Map HD.HdAccountId (Utxo, [HD.HdAddress])
        genDollarUtxo = M.unionsWith (<>) $ flip evalState (M.singleton rootId esk) $ do
            fmap (fmap byAccount . M.toList) $ state $
                prefilterDollarUtxo (selectPrefixDollar $ M.assocs $ genesisUtxo $ configGenesisData coreConfig)
              where
                byAccount (txin, (hdAddr, coin)) =
                    M.singleton accId (utxo, [hdAddr])
                  where
                    accId = hdAddr ^. HD.hdAddressId . HD.hdAddressIdParent
                    addr = hdAddr ^. HD.hdAddressAddress . fromDb
                    utxo = M.singleton txin (TxOutAux $ TxOutDollar addr coin)
    let genUtxo = M.unionsWith (<>) [genSealUtxo, genGoldUtxo, genDollarUtxo]
    -- Get the tip
    let gh = configGenesisHash coreConfig
    mTip <- mostRecentMainBlock gh tipHeader
    case mTip of
        Nothing  ->
            return (WalletCreate genUtxo)
        Just tip ->
            WalletRestore (mergeInfo curUtxo genUtxo) <$> mainBlockContext gh tip

  where
    toPrefilteredUtxo :: Map TxIn (TxOutAux, HD.HdAddressId) -> (Utxo, [HD.HdAddress])
    toPrefilteredUtxo utxoWithAddrs = (M.fromList utxoL, addrs)
      where
        toUtxo (txIn,(txOutAux,_))         = (txIn,txOutAux)
        toAddr (_   ,(txOutAux,addressId)) = HD.initHdAddress addressId (txOutAddress . toaOut $ txOutAux)

        toSummary :: (TxIn,(TxOutAux,HD.HdAddressId))
                  -> ((TxIn,TxOutAux),HD.HdAddress)
        toSummary item = (toUtxo item, toAddr item)

        utxoSummary = map toSummary $ M.toList utxoWithAddrs
        (utxoL, addrs) = unzip utxoSummary

    mergeInfo :: (Monoid cur, Monoid gen)
              => Map HD.HdAccountId (cur, [HD.HdAddress])
              -> Map HD.HdAccountId (gen, [HD.HdAddress])
              -> Map HD.HdAccountId (cur, gen, [HD.HdAddress])
    mergeInfo = M.merge
        (M.mapMaybeMissing     $ \_ (c, as) -> Just (c, mempty, as))
        (M.mapMaybeMissing     $ \_ (g, as) -> Just (mempty, g, as))
        (M.zipWithMaybeMatched $ \_ (c, as) (g, as') -> Just (c, g, as ++ as'))

    mergeUtxos
        :: [(HD.HdAccountId, Map TxIn (TxOutAux, HD.HdAddressId))]
        -> Map HD.HdAccountId (Map TxIn (TxOutAux, HD.HdAddressId))
    mergeUtxos = M.fromListWith M.union

    isOurs :: (TxIn, TxOutAux) -> Maybe (HD.HdAccountId, Map TxIn (TxOutAux, HD.HdAddressId))
    isOurs (inp, out@(TxOutAux (TxOutSeal addr _ _))) = do
        hdAddr <- evalState (state $ HD.isOurs addr) (M.singleton rootId esk)
        let addrId = hdAddr ^. HD.hdAddressId
        return (addrId ^. HD.hdAddressIdParent, M.singleton inp (out, addrId))
    isOurs (inp, out@(TxOutAux (TxOutGold addr _))) = do
        hdAddr <- evalState (state $ HD.isOurs addr) (M.singleton rootId esk)
        let addrId = hdAddr ^. HD.hdAddressId
        return (addrId ^. HD.hdAddressIdParent, M.singleton inp (out, addrId))
    isOurs (inp, out@(TxOutAux (TxOutDollar addr _))) = do
        hdAddr <- evalState (state $ HD.isOurs addr) (M.singleton rootId esk)
        let addrId = hdAddr ^. HD.hdAddressId
        return (addrId ^. HD.hdAddressIdParent, M.singleton inp (out, addrId))
    isOurs (inp, out@(TxOutAux (TxOutState addr _ _))) = do
        hdAddr <- evalState (state $ HD.isOurs addr) (M.singleton rootId esk)
        let addrId = hdAddr ^. HD.hdAddressId
        return (addrId ^. HD.hdAddressIdParent, M.singleton inp (out, addrId))

-- | Restore a wallet's transaction history.
--
-- TODO: Think about what we should do if a 'RestorationException' is thrown.
restoreWalletHistoryAsync
    :: Kernel.PassiveWallet
    -> HD.HdRootId
    -> (Blund -> IO (PrefilteredBlock, [TxMeta]))
    -> IORef WalletRestorationProgress
    -> Maybe HeaderHash
    -- ^ The last hash that this wallet has already restored, or Nothing to
    -- start from the genesis block's successor.
    -> (HeaderHash, SlotId)
    -- ^ The block that we are trying to reach via restoration.
    -> IO ()
restoreWalletHistoryAsync wallet rootId prefilter progress start (tgtHash, tgtSlot) = do
    genesisHash <- configGenesisHash <$> getCoreConfig (wallet ^. walletNode)
    -- 'getFirstGenesisBlockHash' is confusingly named: it returns the hash of
    -- the first block /after/ the genesis block.
    startingPoint <- case start of
        Nothing -> withNode $ getFirstGenesisBlockHash genesisHash
        Just sh -> do
            mhash <- nextHistoricalHash sh
            maybe (throwM $ RestorationSuccessorNotFound sh) pure mhash
    let batchSize = 1000
    restore genesisHash startingPoint batchSize
  where
    -- Process the restoration of the next `batchSize` blocks (or up until the
    -- target hash), starting from the given 'HeaderHash'.
    restore :: GenesisHash -> HeaderHash -> Int -> IO ()
    restore genesisHash hh batchSize = do
        -- Keep track of timing
        startTime <- getCurrentTime

        -- Processing a batch of blocks, flushing to Acid-State only once
        (updates, hh') <- getBatch genesisHash hh batchSize

        -- Flush to Acid-State & SQLite
        k <- getSecurityParameter (wallet ^. walletNode)
        let (blocks, txMetas, slotIds) = unzip3 updates
        mErr <- update (wallet ^. wallets) (ApplyHistoricalBlocks k rootId blocks)
        whenLeft mErr (throwM . RestorationApplyHistoricalBlockFailed)
        forM_ (mconcat txMetas) (putTxMeta (wallet ^. walletMeta))

        -- Update our progress
        now <- getCurrentTime
        slotCount <- getSlotCount (wallet ^. walletNode)
        let flat =
                flattenSlotId slotCount
            rate =
                Rate (fromIntegral $ length blocks) (now `diffUTCTime` startTime)
            blockPerSec =
                MeasuredIn $ BlockCount $ perSecond rate
        unless (null slotIds) $ modifyIORef' progress
            ( (wrpCurrentSlot .~ flat (Prelude.last slotIds))
            . (wrpTargetSlot  .~ flat tgtSlot)
            . set wrpThroughput blockPerSec
            )

        -- Decide how to proceed.
        if hh' == tgtHash then finish else restore genesisHash hh' batchSize

    -- TODO (@mn): probably should use some kind of bracket to ensure this
    -- cleanup happens.
    finish :: IO ()
    finish = do
        k <- getSecurityParameter (wallet ^. walletNode)
        update (wallet ^. wallets) $ RestorationComplete k rootId
        removeRestoration wallet rootId

    -- Step forward to the successor of the given block.
    nextHistoricalHash :: HeaderHash -> IO (Maybe HeaderHash)
    nextHistoricalHash hh = withNode $ resolveForwardLink hh

    -- Get a block
    getBlockOrThrow :: GenesisHash -> HeaderHash -> IO Block
    getBlockOrThrow genesisHash hh = do
        mBlock <- withNode $ getBlock genesisHash hh
        case mBlock of
            Nothing -> throwM $ RestorationBlockNotFound hh
            Just b  -> return b

    -- Get undo for a mainblock
    -- NOTE: We use this undo information only for input resolution.
    getUndoOrThrow :: GenesisHash -> HeaderHash -> IO Undo
    getUndoOrThrow genesisHash hh = do
        mBlock <- withNode $ getUndo genesisHash hh
        case mBlock of
            Nothing -> throwM $ RestorationUndoNotFound hh
            Just b  -> return b

    -- Get prefilter blocks and associated meta
    getPrefilteredBlockOrThrow
        :: GenesisHash
        -> HeaderHash
        -> IO (Maybe BatchData)
    getPrefilteredBlockOrThrow genesisHash hh = do
        block <- getBlockOrThrow genesisHash hh
        case block of
            Left _   -> return Nothing -- Skip EBBs
            Right mb -> do
                -- Filter the blocks by account
                blund <- (block, ) <$> (getUndoOrThrow genesisHash hh)
                (prefilteredBlocks, txMetas) <- prefilter blund
                ctxt <- withNode $ mainBlockContext genesisHash mb
                let slotId = mb ^. mainBlockSlot
                return $ Just ((ctxt, prefilteredBlocks), txMetas, slotId)

    -- Get the next batch of blocks
    getBatch
        :: GenesisHash
        -> HeaderHash
        -> Int
        -> IO ([BatchData], HeaderHash)
    getBatch genesisHash hh batchSize = go [] batchSize hh
        where
            go !updates !n !currentHash | n <= 0 =
                return (reverse updates, currentHash)

            go !updates _ !currentHash | currentHash == tgtHash =
                getPrefilteredBlockOrThrow genesisHash currentHash >>= \case
                    Nothing -> go updates 0 currentHash
                    Just u  -> go (u : updates) 0 currentHash

            go !updates !n !currentHash = do
                nextHash <- nextHistoricalHash currentHash >>= \case
                    Nothing  -> throwM (RestorationFinishUnreachable tgtHash currentHash)
                    Just hh' -> return hh'

                getPrefilteredBlockOrThrow genesisHash currentHash >>= \case
                    Nothing -> go updates (n - 1) nextHash
                    Just u  -> go (u : updates) (n - 1) nextHash

    withNode :: forall a. (NodeConstraints => WithNodeState IO a) -> IO a
    withNode action = withNodeState (wallet ^. walletNode) (\_lock -> action)

type BatchData =
    ( (BlockContext, PrefilteredBlock)
    , [TxMeta]
    , SlotId
    )

{-------------------------------------------------------------------------------
  Timing information (for throughput calculations)
-------------------------------------------------------------------------------}

-- | A rate, represented as an event count over a time interval.
data Rate = Rate Integer NominalDiffTime

-- | Convert a rate to a number of events per second.
perSecond :: Rate -> Word64
perSecond (Rate n dt) = fromInteger $ round (toRational n / toRational dt)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Exception during restoration
data RestorationException =
    RestorationBlockNotFound HeaderHash
  | RestorationSuccessorNotFound HeaderHash
  | RestorationUndoNotFound HeaderHash
  | RestorationApplyHistoricalBlockFailed Spec.ApplyBlockFailed
  | RestorationFinishUnreachable HeaderHash HeaderHash

instance Buildable RestorationException where
    build (RestorationBlockNotFound hash) =
        bprint ("RestorationBlockNotFound " % build) hash
    build (RestorationSuccessorNotFound hash) =
        bprint ("RestorationSuccessorNotFound " % build) hash
    build (RestorationUndoNotFound hash) =
        bprint ("RestorationUndoNotFound " % build) hash
    build (RestorationApplyHistoricalBlockFailed err) =
        bprint ("RestorationApplyHistoricalBlockFailed " % build) err
    build (RestorationFinishUnreachable target final) =
        bprint ("RestorationFinishUnreachable " % build % " " % build) target final

instance Show RestorationException where
    show = formatToString build

instance Exception RestorationException

{-------------------------------------------------------------------------------
  Shared with Cardano.Wallet.WalletLayer.Kernel.Wallets
-------------------------------------------------------------------------------}

-- | The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
-- by the invariants established in the 'Blund'.
blundToResolvedBlock :: NodeStateAdaptor IO -> Blund -> IO (Maybe ResolvedBlock)
blundToResolvedBlock node (b,u) = do
    genesisHash <- configGenesisHash <$> getCoreConfig node
    case b of
        Left  _ebb      -> return Nothing
        Right mainBlock -> withNodeState node $ \_lock -> do
            ctxt  <- mainBlockContext genesisHash mainBlock
            mTime <- defaultGetSlotStart (mainBlock ^. mainBlockSlot)
            now   <- liftIO $ getCurrentTimestamp
            return $ Just $ fromRawResolvedBlock UnsafeRawResolvedBlock {
                rawResolvedBlock       = mainBlock
              , rawResolvedBlockInputs = map (map fromJust) $ undoTx u
              , rawTimestamp           = either (const now) identity mTime
              , rawResolvedContext     = ctxt
              }

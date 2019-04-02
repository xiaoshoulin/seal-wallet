{-# LANGUAGE LambdaCase #-}

-- | Resolved blocks and transactions
module Seal.Wallet.Kernel.DB.Resolved
    ( -- * Resolved blocks and transactions
      ResolvedTx(..)
    , ResolvedBlock(..)

      -- * Lenses
    , rtxInputs
    , rtxOutputs
    , rtxAccountInputs
    , rtxAccountOutputs
    , rtxCmds
    , rtxMeta
    , rbTxs
    , rbContext
    , rbMeta

    , isSealInput
    , isGoldInput
    , isDollarInput
    , isSealUtxo
    , isGoldUtxo
    , isDollarUtxo
      -- * Metadata
    , resolvedToTxMetas
    ) where

import           Universum hiding (truncate)

import           Control.Lens.TH (makeLenses)
import qualified Data.List as List
import qualified Data.Map as Map
import           Formatting (bprint, (%))
import qualified Formatting.Buildable

import           Serokell.Util (listJson, mapJson, pairF)

import           Seal.Wallet.Core (unsafeAddCoinGroup)
import           Seal.Wallet.Kernel.DB.BlockContext (BlockContext)
import           Seal.Wallet.Kernel.DB.HdWallet (HdAccountId (..),
                     HdAccountIx (..), IsOurs (..), hdAccountIdIx,
                     hdAccountIdParent, hdAddressId, hdAddressIdParent)
import           Seal.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Seal.Wallet.Kernel.DB.TxMeta.Types (TxMeta (..))
import           Seal.Wallet.Kernel.Util.Core (absCoinGroup, sumCoinsUnsafe,
                     sumGoldCoinsUnsafe, sumGoldDollarsUnsafe,
                     sumCoinGroupsUnsafe)
import           Seal.Chain.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..),
                     Utxo, Nonce, isSealTxOut, isGoldTxOut, isDollarTxOut)
import           Seal.Core (Address, Coin (..), SlotId, Timestamp, GoldDollar (..),
                     GoldCoin (..), CoinGroup (..), Cmd)


{-------------------------------------------------------------------------------
  Resolved blocks and transactions
-------------------------------------------------------------------------------}

-- | (Unsigned) transaction with inputs resolved
--
-- NOTE: We cannot recover the original transaction from a 'ResolvedTx'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedTx = ResolvedTx {
      -- | Transaction inputs
      -- A transaction input @(h, i)@ points to the @i@th output of the transaction
      -- with hash @h@, which is not particularly informative. The corresponding
      -- 'ResolvedInput' is obtained by looking up what that output actually is.
      _rtxInputs  :: InDb [(TxIn, TxOutAux)]

      -- | Transaction outputs
    , _rtxOutputs :: InDb Utxo

      -- 
    , _rtxAccountInputs :: InDb [(Address, Nonce, CoinGroup)]

      --
    , _rtxAccountOutputs :: InDb [(Address, CoinGroup)]


    , _rtxCmds           :: InDb [Cmd]

     -- | Transaction Meta
    , _rtxMeta    :: InDb (TxId, Timestamp)
    }

-- | (Unsigned block) containing resolved transactions
--
-- NOTE: We cannot recover the original block from a 'ResolvedBlock'.
-- Any information needed inside the wallet kernel must be explicitly
-- represented here.
data ResolvedBlock = ResolvedBlock {
      -- | Transactions in the block
      _rbTxs     :: ![ResolvedTx]

      -- | Block context
    , _rbContext :: !BlockContext

      -- | Creation time of this block
    , _rbMeta    :: !Timestamp
    }

makeLenses ''ResolvedTx
makeLenses ''ResolvedBlock


{-------------------------------------------------------------------------------
  Metadata
-------------------------------------------------------------------------------}

-- | Error returned whenever
data ErrMalformedResolvedBlock = ErrMalformedResolvedBlock Text
    deriving Show
instance Exception ErrMalformedResolvedBlock


resolvedToTxMetas
    :: (IsOurs s)
    => ResolvedBlock
    -> s
    -> (Either ErrMalformedResolvedBlock [TxMeta], s)
resolvedToTxMetas rb = runState $ runExceptT $ fmap mconcat $ do
    forM (rb ^. rbTxs) $ \tx -> do
        accounts <- rtxAccountInfo tx
        forM accounts $ \(accId, spent, gained, isLocal) -> ExceptT $ return $ do
            inps  <- traverse mkTxMetaInps $ filter isSealInput (tx ^. rtxInputs . fromDb)
            outs  <- mkTxMetaOuts (tx ^. rtxOutputs . fromDb)
            ginps <- traverse mkTxMetaGoldInps $ filter isGoldInput (tx ^. rtxInputs . fromDb)
            gouts <- mkTxMetaGoldOuts (tx ^. rtxOutputs . fromDb)
            dinps <- traverse mkTxMetaDollarInps $ filter isDollarInput (tx ^. rtxInputs . fromDb)
            douts <- mkTxMetaDollarOuts (tx ^. rtxOutputs . fromDb)
            ainps <- traverse mkTxMetaAccountInps (tx ^. rtxAccountInputs . fromDb)
            aouts <- traverse mkTxMetaAccountOuts (tx ^. rtxAccountOutputs . fromDb)
            cmds  <- traverse mkTxMetaCmds (tx ^. rtxCmds . fromDb)
            return TxMeta 
                { _txMetaId = tx ^. rtxMeta . fromDb . _1
                , _txMetaAmount = absCoinGroup spent gained
                , _txMetaInputs = inps
                , _txMetaOutputs = outs
                , _txMetaGoldInputs = ginps
                , _txMetaGoldOutputs = gouts      
                , _txMetaDollarInputs = dinps
                , _txMetaDollarOutputs = douts
                , _txMetaAccountInputs = ainps 
                , _txMetaAccountOutputs = aouts
                , _txMetaCmds = cmds
                , _txMetaCreationAt = tx ^. rtxMeta . fromDb . _2
                , _txMetaIsLocal = isLocal
                , _txMetaIsOutgoing = gained < spent
                , _txMetaWalletId = accId ^. hdAccountIdParent
                , _txMetaAccountIx = getHdAccountIx $ accId ^. hdAccountIdIx
                }
  where
      -- | Inspect a resolved transaction in a given context:
      --  - @accId@ for the underlying account id
      --  - @spent@ coins from input addrs; reduce the balance.
      --  - @gained@ coins from output addrs; increase the balance.
      --  - @isLocal@ true if all inputs and outputs addrs belong to the account.
      -- Note that those values aren't absolute for it depends on _who_ inspects
      -- the transaction.
      -- This is why we may obtain multiple meta from a single transaction but
      -- at most, one per account id we recognized as being ours.
    rtxAccountInfo
        :: (IsOurs s, MonadState s m)
        => ResolvedTx
        -> m [(HdAccountId, CoinGroup, CoinGroup, Bool)]
    rtxAccountInfo tx = do
        let inps = fmap snd $ tx ^. rtxInputs . fromDb
        ourInps  <- ours  (\coins -> ([coins], []))             $ toResolvedSeal inps
        gourInps <- gours (\goldcoins -> ([goldcoins], []))     $ toResolvedGold inps
        dourInps <- dours (\golddollars -> ([golddollars], [])) $ toResolvedDollar inps

        let outs = Map.elems $ tx ^. rtxOutputs . fromDb
        ourOuts  <- ours (\coins -> ([], [coins]))              $ toResolvedSeal outs
        gourOuts <- gours (\goldcoins -> ([], [goldcoins]))     $ toResolvedGold outs
        dourOuts <- dours (\golddollars -> ([], [golddollars])) $ toResolvedDollar outs

        let ainps = tx ^. rtxAccountInputs . fromDb
        let aouts = tx ^. rtxAccountOutputs . fromDb
        aourInps  <- aours  (\coingroups -> ([coingroups], [])) $ noNonce ainps
        aourOuts  <- aours  (\coingroups -> ([], [coingroups])) aouts

        let isLocal = (length inps + length outs + length ainps + length aouts)
                      ==
                      (length ourInps + length ourOuts + length gourInps + length gourOuts + length dourInps + length dourOuts + length aourInps + length aourOuts)
       
        let outmaps  = flip Map.map (Map.unionWith (<>) ourInps ourOuts)   $ 
                          (\(ourInps', ourOuts') -> (CoinGroup (sumCoinsUnsafe ourInps') (GoldCoin 0) (GoldDollar 0), CoinGroup (sumCoinsUnsafe ourOuts') (GoldCoin 0) (GoldDollar 0)))
        let goutmaps = flip Map.map (Map.unionWith (<>) gourInps gourOuts) $ 
                          (\(gourInps', gourOuts') -> (CoinGroup (Coin 0) (sumGoldCoinsUnsafe gourInps') (GoldDollar 0), CoinGroup (Coin 0) (sumGoldCoinsUnsafe gourOuts') (GoldDollar 0))) 
        let doutmaps = flip Map.map (Map.unionWith (<>) dourInps dourOuts) $ 
                          (\(dourInps', dourOuts') -> (CoinGroup (Coin 0) (GoldCoin 0) (sumGoldDollarsUnsafe dourInps'), CoinGroup (Coin 0) (GoldCoin 0) (sumGoldDollarsUnsafe dourOuts')))
        let aoutmaps = flip Map.map (Map.unionWith (<>) aourInps aourOuts) $ 
                          (\(aourInps', aourOuts') -> (sumCoinGroupsUnsafe aourInps', sumCoinGroupsUnsafe aourOuts'))
        let allmaps  = flip unionMaps [outmaps, goutmaps, doutmaps, aoutmaps] $
                          (\(cgInps, cgOuts) (cgInps', cgOuts') -> (unsafeAddCoinGroup cgInps cgInps', unsafeAddCoinGroup cgOuts cgOuts'))
        return $ flip map (Map.toList allmaps) $ \(accId, (cgInps, cgOuts)) -> (accId, cgInps, cgOuts, isLocal) 
        
    ours
        :: (Traversable t, IsOurs s, MonadState s m, Semigroup b)
        => (Coin -> b)
        -> t (Address, Coin, Maybe SlotId)
        -> m (Map HdAccountId b)
    ours fn = flip foldM mempty $ \m (addr, coin, _) -> do
        state (isOurs addr) <&> \case
            Nothing -> m
            Just x  -> Map.unionWith (<>) m $ Map.singleton (x ^.  hdAddressId ^. hdAddressIdParent) (fn coin)

    gours
        :: (Traversable t, IsOurs s, MonadState s m, Semigroup b)
        => (GoldCoin -> b)
        -> t (Address, GoldCoin)
        -> m (Map HdAccountId b)
    gours fn = flip foldM mempty $ \m (addr, gcoin) -> do
        state (isOurs addr) <&> \case
            Nothing -> m
            Just x  -> Map.unionWith (<>) m $ Map.singleton (x ^.  hdAddressId ^. hdAddressIdParent) (fn gcoin)
            
    dours
        :: (Traversable t, IsOurs s, MonadState s m, Semigroup b)
        => (GoldDollar -> b)
        -> t (Address, GoldDollar)
        -> m (Map HdAccountId b)
    dours fn = flip foldM mempty $ \m (addr, dcoin) -> do
        state (isOurs addr) <&> \case
            Nothing -> m
            Just x  -> Map.unionWith (<>) m $ Map.singleton (x ^.  hdAddressId ^. hdAddressIdParent) (fn dcoin)

    aours
        :: (Traversable t, IsOurs s, MonadState s m, Semigroup b)
        => (CoinGroup -> b)
        -> t (Address, CoinGroup)
        -> m (Map HdAccountId b)
    aours fn = flip foldM mempty $ \m (addr, coingroup) -> do
        state (isOurs addr) <&> \case
            Nothing -> m 
            Just x  -> Map.unionWith (<>) m $ Map.singleton (x ^.  hdAddressId ^. hdAddressIdParent) (fn coingroup)

    mkTxMetaOuts
        :: Utxo
        -> Either ErrMalformedResolvedBlock [(Address, Coin, Maybe SlotId)]
    mkTxMetaOuts utxo = utxo
        & Map.elems
        & toResolvedSeal
        & Right

    mkTxMetaGoldOuts
        :: Utxo
        -> Either ErrMalformedResolvedBlock [(Address, GoldCoin)]
    mkTxMetaGoldOuts utxo = utxo
        & Map.elems
        & toResolvedGold
        & Right

    mkTxMetaDollarOuts
        :: Utxo
        -> Either ErrMalformedResolvedBlock [(Address, GoldDollar)]
    mkTxMetaDollarOuts utxo = utxo
        & Map.elems
        & toResolvedDollar
        & Right

    mkTxMetaInps
        :: (TxIn, TxOutAux)
        -> Either ErrMalformedResolvedBlock (TxId, Word32, Address, Coin, Maybe SlotId)
    mkTxMetaInps (txin, TxOutAux (TxOutSeal addr coin lock)) =
        uncurry (,,addr,coin,lock) <$> case txin of
            TxInUtxo txId ix -> Right (txId, ix)
            TxInUnknown{} ->
                Left $ ErrMalformedResolvedBlock $ "Tx input is unknown: " <> show txin
    mkTxMetaInps (_, TxOutAux (_)) =
        Left $ ErrMalformedResolvedBlock $ "Tx output is not SealTxOut"

    mkTxMetaGoldInps
        :: (TxIn, TxOutAux)
        -> Either ErrMalformedResolvedBlock (TxId, Word32, Address, GoldCoin)
    mkTxMetaGoldInps (txin, TxOutAux (TxOutGold addr coin)) =
        uncurry (,,addr,coin) <$> case txin of
            TxInUtxo txId ix -> Right (txId, ix)
            TxInUnknown{} ->
                Left $ ErrMalformedResolvedBlock $ "Tx input is unknown: " <> show txin
    mkTxMetaGoldInps (_, TxOutAux (_)) =
        Left $ ErrMalformedResolvedBlock $ "Tx output is not GoldTxOut"

    mkTxMetaDollarInps
        :: (TxIn, TxOutAux)
        -> Either ErrMalformedResolvedBlock (TxId, Word32, Address, GoldDollar)
    mkTxMetaDollarInps (txin, TxOutAux (TxOutDollar addr coin)) =
        uncurry (,,addr,coin) <$> case txin of
            TxInUtxo txId ix -> Right (txId, ix)
            TxInUnknown{} ->
                Left $ ErrMalformedResolvedBlock $ "Tx input is unknown: " <> show txin
    mkTxMetaDollarInps (_, TxOutAux (_)) =
        Left $ ErrMalformedResolvedBlock $ "Tx output is not DollarTxOut"

    mkTxMetaAccountInps
        :: (Address, Nonce, CoinGroup)
        -> Either ErrMalformedResolvedBlock (Address, Nonce, CoinGroup)
    mkTxMetaAccountInps (a, n, c) = Right (a, n, c)

    mkTxMetaAccountOuts
        :: (Address, CoinGroup)
        -> Either ErrMalformedResolvedBlock (Address, CoinGroup)
    mkTxMetaAccountOuts (a, c) = Right (a, c)

    mkTxMetaCmds
        :: Cmd
        -> Either ErrMalformedResolvedBlock Cmd
    mkTxMetaCmds c = Right c

isSealInput :: (TxIn, TxOutAux) -> Bool
isSealInput (_, txOutAux) = isSealTxOut $ toaOut txOutAux

isGoldInput :: (TxIn, TxOutAux) -> Bool
isGoldInput (_, txOutAux) = isGoldTxOut $ toaOut txOutAux

isDollarInput :: (TxIn, TxOutAux) -> Bool
isDollarInput (_, txOutAux) = isDollarTxOut $ toaOut txOutAux

isSealUtxo :: TxOutAux -> Bool
isSealUtxo = isSealTxOut . toaOut 

isGoldUtxo :: TxOutAux -> Bool
isGoldUtxo = isGoldTxOut . toaOut 

isDollarUtxo :: TxOutAux -> Bool
isDollarUtxo = isDollarTxOut . toaOut 

toResolvedSeal :: [TxOutAux] -> [(Address, Coin, Maybe SlotId)]
toResolvedSeal ((TxOutAux (TxOutSeal a c s)):txOuts) = (a, c, s): (toResolvedSeal txOuts)
toResolvedSeal (_:txOuts)                            = toResolvedSeal txOuts
toResolvedSeal []                                    = []
    
toResolvedGold :: [TxOutAux] -> [(Address, GoldCoin)]
toResolvedGold ((TxOutAux (TxOutGold a c)):txOuts) = (a, c): (toResolvedGold txOuts)
toResolvedGold (_:txOuts)                          = toResolvedGold txOuts
toResolvedGold []                                  = []

toResolvedDollar :: [TxOutAux] -> [(Address, GoldDollar)]
toResolvedDollar ((TxOutAux (TxOutDollar a c)):txOuts) = (a, c): (toResolvedDollar txOuts)
toResolvedDollar (_:txOuts)                            = toResolvedDollar txOuts
toResolvedDollar []                                    = []

unionMaps :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a
unionMaps fn maps = go fn maps mempty
  where
    go :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a -> Map k a
    go _ [] m  = m
    go fun ms m = go fun (List.tail ms) $ Map.unionWith fun (List.head ms) m

noNonce :: [(Address, Nonce, CoinGroup)] -> [(Address, CoinGroup)]
noNonce acc = flip map acc $ \(a, _, c) -> (a, c)
{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable ResolvedTx where
  build ResolvedTx{..} = bprint
    ( "ResolvedTx "
    % "{ inputs:  "        % mapJson
    % ", outputs: "        % mapJson
    -- % ", accountinputs: "  % listJson
    -- % ", accountoutputs: " % listJson
    -- % ", cmd: "            % listJson
    % ", meta:    "        % pairF
    % "}"
    )
    (Map.fromList (toList (_rtxInputs  ^. fromDb)))
    (_rtxOutputs ^. fromDb)
    -- (_rtxAccountInputs  ^. fromDb)
    -- (_rtxAccountOutputs  ^. fromDb)
    -- (_rtxCmds  ^. fromDb)
    (_rtxMeta ^. fromDb)

instance Buildable ResolvedBlock where
  build ResolvedBlock{..} = bprint
    ( "ResolvedBlock "
    % "{ txs: " % listJson
    % "}"
    )
    _rbTxs

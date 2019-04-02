{-# LANGUAGE LambdaCase #-}

module Seal.Wallet.Kernel.Prefiltering
    ( -- * Types
      PrefilteredBlock
    , PrefilteredTx

    -- * Constructors
    , prefilterBlock
    , selectPrefixSeal
    , selectPrefixGold
    , selectPrefixDollar
    , prefilterUtxo
    , prefilterGoldUtxo
    , prefilterDollarUtxo

    -- * Smart Getters @PrefilteredTx@
    , pftInputs
    , pftGoldInputs
    , pftDollarInputs
    , pftMeta
    , pftOutputs
    , pftGoldOutputs
    , pftDollarOutputs

    -- * Smart Getters @PrefilteredBlock@
    , pfbAddrs
    , pfbByAccounts
    , pfbInputs
    , pfbGoldInputs
    , pfbDollarInputs
    , pfbMeta
    , pfbOutputs
    , pfbGoldOutputs
    , pfbDollarOutputs
    ) where

import           Universum

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Set as Set

import           Seal.Wallet.Kernel.DB.BlockContext (BlockContext, bcSlotId)
import           Seal.Wallet.Kernel.DB.BlockMeta (AddressMeta,
                     BlockMeta (..), LocalBlockMeta (..), addressMetaIsUsed)
import           Seal.Wallet.Kernel.DB.HdWallet (HdAccountId (..),
                     HdAddress (..), HdAddressId (..), IsOurs (..))
import           Seal.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Seal.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbTxs,
                     rtxInputs, rtxMeta, rtxOutputs)
import           Seal.Chain.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..),
                     Utxo)
import           Seal.Core (Address, Coin, GoldCoin, GoldDollar, SlotId, Timestamp)


{-------------------------------------------------------------------------------
    Types
-------------------------------------------------------------------------------}

data PrefilteredTx = PrefilteredTx
    { pftInputs -- | Relevant inputs
        :: !(Map TxIn (HdAddress, Coin, Maybe SlotId))

    , pftOutputs -- | Relevant outputs
        :: !(Map TxIn (HdAddress, Coin, Maybe SlotId))
    
    , pftGoldInputs -- | Relevant inputs
        :: !(Map TxIn (HdAddress, GoldCoin))

    , pftGoldOutputs -- | Relevant outputs
        :: !(Map TxIn (HdAddress, GoldCoin))

    , pftDollarInputs -- | Relevant inputs
        :: !(Map TxIn (HdAddress, GoldDollar))

    , pftDollarOutputs -- | Relevant outputs
        :: !(Map TxIn (HdAddress, GoldDollar))

    , pftMeta -- | Transaction Meta
        :: InDb (TxId, Timestamp)
    } deriving (Eq)
deriveSafeCopy 0 'base ''PrefilteredTx

-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock
    { pfbTxs -- | All Prefiltered Transactions
        :: ![PrefilteredTx]

    , pfbForeigns -- | Foreign Inputs
        :: !(Map HdAccountId (Set TxIn))
    } deriving (Eq)
deriveSafeCopy 0 'base ''PrefilteredBlock

instance Semigroup PrefilteredBlock where
    (PrefilteredBlock t1 f1) <> (PrefilteredBlock t2 f2) =
        PrefilteredBlock (t1 <> t2) (f1 <> f2)

instance Monoid PrefilteredBlock where
    mempty = PrefilteredBlock mempty mempty


{-------------------------------------------------------------------------------
    Constructors
-------------------------------------------------------------------------------}

-- | Prefilter a resolved block, grouping by account ids. This discards
-- outputs that and inputs that are irrelevant for the block.
--
-- NOTE
-- Resolved inputs are treated like outputs. In essence, inputs are actually the
-- outputs of previous transactions. So, filtering is (almost) symmetric here.
-- We do consider (and retain) foreign inputs however.
prefilterBlock
    :: IsOurs s
    => Map HdAccountId (Set TxIn)
    -> ResolvedBlock
    -> s
    -> (PrefilteredBlock, s)
prefilterBlock foreigns rb = runState $ do
    PrefilteredBlock <$> prefilteredTxs <*> pure foreigns
  where
    prefilteredTxs = forM (rb ^. rbTxs) $ \tx -> do
        inputs   <- state $ prefilterUtxo $ selectPrefixSeal $ tx ^. rtxInputs . fromDb
        outputs  <- state $ prefilterUtxo $ selectPrefixSeal $ Map.assocs $ tx ^. rtxOutputs . fromDb
        ginputs  <- state $ prefilterGoldUtxo $ selectPrefixGold $ tx ^. rtxInputs . fromDb
        goutputs <- state $ prefilterGoldUtxo $ selectPrefixGold $ Map.assocs $ tx ^. rtxOutputs . fromDb
        dinputs  <- state $ prefilterDollarUtxo $ selectPrefixDollar $ tx ^. rtxInputs . fromDb
        doutputs <- state $ prefilterDollarUtxo $ selectPrefixDollar $ Map.assocs $ tx ^. rtxOutputs . fromDb
        return $ PrefilteredTx inputs outputs ginputs goutputs dinputs doutputs (tx ^. rtxMeta)

selectPrefixSeal :: [(TxIn, TxOutAux)] -> Map TxIn (Address, Coin, Maybe SlotId)
selectPrefixSeal utxo = Map.fromList $ go utxo []
  where
    go :: [(TxIn, TxOutAux)] 
       -> [(TxIn, (Address, Coin, Maybe SlotId))]
       -> [(TxIn, (Address, Coin, Maybe SlotId))]
    go [] l   = l
    go xs l = case L.head xs of 
        (txIn, TxOutAux (TxOutSeal addr coin lock)) -> go (L.tail xs) $ [(txIn, (addr, coin, lock))] ++ l
        (_, _)                                      -> go (L.tail xs) l

selectPrefixGold :: [(TxIn, TxOutAux)] -> Map TxIn (Address, GoldCoin)
selectPrefixGold utxo = Map.fromList $ go utxo []
  where
    go :: [(TxIn, TxOutAux)] 
       -> [(TxIn, (Address, GoldCoin))]
       -> [(TxIn, (Address, GoldCoin))]
    go [] l   = l
    go xs l = case L.head xs of 
        (txIn, TxOutAux (TxOutGold addr coin)) -> go (L.tail xs) $ [(txIn, (addr, coin))] ++ l
        (_, _)                                 -> go (L.tail xs) l

selectPrefixDollar :: [(TxIn, TxOutAux)] -> Map TxIn (Address, GoldDollar)
selectPrefixDollar utxo = Map.fromList $ go utxo []
  where
    go :: [(TxIn, TxOutAux)] 
       -> [(TxIn, (Address, GoldDollar))]
       -> [(TxIn, (Address, GoldDollar))]
    go [] l   = l
    go xs l = case L.head xs of 
        (txIn, TxOutAux (TxOutDollar addr coin)) -> go (L.tail xs) $ [(txIn, (addr, coin))] ++ l
        (_, _)                                   -> go (L.tail xs) l

prefilterUtxo
    :: IsOurs s
    => Map TxIn (Address, Coin, Maybe SlotId)
    -> s
    -> (Map TxIn (HdAddress, Coin, Maybe SlotId), s)
prefilterUtxo utxo = runState $ do
    flip Map.traverseMaybeWithKey utxo $ \_ (addr, coin, lock) ->
        fmap (,coin,lock) <$> state (isOurs addr)

prefilterGoldUtxo
    :: IsOurs s
    => Map TxIn (Address, GoldCoin)
    -> s
    -> (Map TxIn (HdAddress, GoldCoin), s)
prefilterGoldUtxo utxo = runState $
    flip Map.traverseMaybeWithKey utxo $ \_ (addr, coin) ->
        fmap (,coin) <$> state (isOurs addr)

prefilterDollarUtxo
    :: IsOurs s
    => Map TxIn (Address, GoldDollar)
    -> s
    -> (Map TxIn (HdAddress, GoldDollar), s)
prefilterDollarUtxo utxo = runState $
    flip Map.traverseMaybeWithKey utxo $ \_ (addr, coin) ->
        fmap (,coin) <$> state (isOurs addr)


{-------------------------------------------------------------------------------
    Getters / Transformations @PrefilteredBlock@
-------------------------------------------------------------------------------}

pfbAddrs :: PrefilteredBlock -> [HdAddress]
pfbAddrs =
    L.nub . mconcat . fmap (fmap fstt . Map.elems . pftOutputs) . pfbTxs
        where  
            fstt :: (a, b, c) -> a
            fstt (a, _, _) = a

-- | Re-index a 'PrefilteredBlock' per account id, where each corresponding
-- block is tailored to the account it (i.e only contains data relevant to this
-- account).
pfbByAccounts :: PrefilteredBlock -> Map HdAccountId PrefilteredBlock
pfbByAccounts block =
    Map.unionsWith (<>) $ map
        (\tx -> foldl' (byAccount tx) mempty (mapinpout tx))
        (pfbTxs block)
  where
    mapinpout tx = Map.map fst3 (pftOutputs tx) <> 
                   Map.map fst3 (pftInputs tx) <>
                   Map.map fst (pftGoldOutputs tx) <>
                   Map.map fst (pftGoldInputs tx) <>
                   Map.map fst (pftDollarOutputs tx) <>
                   Map.map fst (pftDollarInputs tx)

    fst3 :: (a, b, c) -> a
    fst3 (a, _, _) = a

    byAccount tx m (HdAddress (HdAddressId accId _) _) =
        let
            outs'  = Map.filter ((== accId) . _hdAddressIdParent . _hdAddressId . fst3) (pftOutputs tx)
            inps'  = Map.filter ((== accId) . _hdAddressIdParent . _hdAddressId . fst3) (pftInputs  tx)
            gouts' = Map.filter ((== accId) . _hdAddressIdParent . _hdAddressId . fst) (pftGoldOutputs tx)
            ginps' = Map.filter ((== accId) . _hdAddressIdParent . _hdAddressId . fst) (pftGoldInputs  tx)
            douts' = Map.filter ((== accId) . _hdAddressIdParent . _hdAddressId . fst) (pftDollarOutputs tx)
            dinps' = Map.filter ((== accId) . _hdAddressIdParent . _hdAddressId . fst) (pftDollarInputs  tx)
            txs    = [ tx { pftOutputs       = outs',  
                            pftInputs        = inps', 
                            pftGoldOutputs   = gouts', 
                            pftGoldInputs    = ginps',
                            pftDollarOutputs = douts', 
                            pftDollarInputs  = dinps'
                          } ]
            frgns  = Map.restrictKeys (pfbForeigns block) (Set.singleton accId)
        in
            Map.insert accId (PrefilteredBlock txs frgns) m

pfbInputs :: PrefilteredBlock -> Set TxIn
pfbInputs block = mconcat $ fmap (Map.keysSet . pftInputs) $ pfbTxs block

pfbGoldInputs :: PrefilteredBlock -> Set TxIn
pfbGoldInputs block = mconcat $ fmap (Map.keysSet . pftGoldInputs) $ pfbTxs block

pfbDollarInputs :: PrefilteredBlock -> Set TxIn
pfbDollarInputs block = mconcat $ fmap (Map.keysSet . pftDollarInputs) $ pfbTxs block

pfbMeta :: (BlockContext, PrefilteredBlock) -> LocalBlockMeta
pfbMeta (ctx, block) =
    LocalBlockMeta $ BlockMeta (InDb slotMeta) addressMeta
  where
    slotMeta :: Map TxId SlotId
    slotMeta = Map.fromList $ flip map (pfbTxs block) $ \tx ->
        ( pftMeta tx ^. fromDb . _1
        , ctx ^. bcSlotId . fromDb
        )
    -- An address "addr" is considered "used" if
    --   (a) ours addr
    --   (b) There is at least one tx for which: ∃ c.(addr, c) ∈  outputs
    --
    -- Both (a) & (b) are guaranteed as a result of prefiltering.
    addressMeta :: Map (InDb Address) AddressMeta
    addressMeta = Map.fromList $ flip map (pfbAddrs block) $ \(HdAddress _ addr) ->
        ( addr
        , mempty & addressMetaIsUsed .~ True
        )

pfbOutputs :: PrefilteredBlock -> Utxo
pfbOutputs =
    fmap toTxOutAux . mconcat . fmap pftOutputs . pfbTxs
  where
    toTxOutAux (HdAddress _ (InDb addr), coin, lock) = TxOutAux $ TxOutSeal addr coin lock

pfbGoldOutputs :: PrefilteredBlock -> Utxo
pfbGoldOutputs =
    fmap toTxOutAux . mconcat . fmap pftGoldOutputs . pfbTxs
  where
    toTxOutAux (HdAddress _ (InDb addr), coin) = TxOutAux $ TxOutGold addr coin

pfbDollarOutputs :: PrefilteredBlock -> Utxo
pfbDollarOutputs =
    fmap toTxOutAux . mconcat . fmap pftDollarOutputs . pfbTxs
  where
    toTxOutAux (HdAddress _ (InDb addr), coin) = TxOutAux $ TxOutDollar addr coin

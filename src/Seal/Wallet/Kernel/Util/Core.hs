{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
-- | Utility functions on core types
--
-- Intended for qualified import
--
-- > import qualified Seal.Wallet.Kernel.Util.Core as Core
module Seal.Wallet.Kernel.Util.Core (
    -- * General utility functions
    absCoin
  , absGoldCoin
  , absGoldDollar
  , absCoinGroup
  , getCurrentTimestamp
  , sumCoinsUnsafe
  , sumGoldCoinsUnsafe
  , sumGoldDollarsUnsafe
  , sumCoinGroupsUnsafe
    -- * UTxO
  , utxoBalance
  , utxoGoldBalance
  , utxoDollarBalance
  , utxoRestrictToInputs
  , utxoRemoveInputs
  , utxoUnions
  , toAddress
    -- * Transactions
  , paymentAmount
  , toCoin
  , toGoldCoin
  , toGoldDollar
  , txOuts
  , txIns
  , txAuxId
  ) where

import           Universum

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Serokell.Util (enumerate)

import qualified Seal.Chain.Txp as Core
import qualified Seal.Core as Core
import           Seal.Crypto.Hashing (hash)
import           Seal.Wallet.Core

-- import           UTxO.Util

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

-- | Get current timestamp
--
-- NOTE: we are abandoning the 'Mockable time' strategy of core.
getCurrentTimestamp :: IO Core.Timestamp
getCurrentTimestamp = Core.Timestamp . round . (* 1000000) <$> getPOSIXTime

{-------------------------------------------------------------------------------
  UTxO
-------------------------------------------------------------------------------}

-- | Computes the balance for this UTxO
--
-- This returns an 'Integer' rather than a 'Coin' because the outputs of a
-- block may sum to more than 'maxCoinVal' (if some outputs of the transactions
-- in the block are used as inputs by other transactions in that block).
utxoBalance :: Core.Utxo -> Integer
utxoBalance = foldl' updateFn 0 . Map.elems
  where
    updateFn :: Integer -> Core.TxOutAux -> Integer
    updateFn acc txOut = acc + coinToInteger (toCoin txOut) 

utxoGoldBalance :: Core.Utxo -> Integer
utxoGoldBalance = foldl' updateFn 0 . Map.elems
  where
    updateFn :: Integer -> Core.TxOutAux -> Integer
    updateFn acc txOut = acc + goldCoinToInteger (toGoldCoin txOut) 

utxoDollarBalance :: Core.Utxo -> Integer
utxoDollarBalance = foldl' updateFn 0 . Map.elems
  where
    updateFn :: Integer -> Core.TxOutAux -> Integer
    updateFn acc txOut = acc + goldDollarToInteger (toGoldDollar txOut) 


-- | Restricts the 'Utxo' to only the selected set of inputs.
utxoRestrictToInputs :: Core.Utxo -> Set Core.TxIn -> Core.Utxo
utxoRestrictToInputs = restrictKeys

utxoRemoveInputs :: Core.Utxo -> Set Core.TxIn -> Core.Utxo
utxoRemoveInputs = withoutKeys

utxoUnions :: [Core.Utxo] -> Core.Utxo
utxoUnions = Map.unions

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

-- | Calculates the amount of a requested payment.
paymentAmount :: [Core.AccountIn] -> [Core.TxOut] -> Core.CoinGroup
paymentAmount acc utxo = absCoinGroup (sumCoinGroupsUnsafe $ map Core.riValue acc) $ 
               Core.CoinGroup
                  (unsafeIntegerToCoin $ sumCoins $ resolveTxOutSeal utxo)
                  (unsafeIntegerToGoldCoin $ sumGoldCoins $ resolveTxOutGold utxo) 
                  (unsafeIntegerToGoldDollar $ sumGoldDollars $ resolveTxOutDollar utxo) 

resolveTxOutSeal :: [Core.TxOut] -> [Core.Coin]
resolveTxOutSeal x = go x []
  where
    go :: [Core.TxOut] -> [Core.Coin] -> [Core.Coin]
    go [] coins = coins
    go xs coins = case List.head xs of
      Core.TxOutSeal _ coin _ -> go (List.tail xs) ([coin] <> coins)
      _                       -> go (List.tail xs) coins

resolveTxOutGold :: [Core.TxOut] -> [Core.GoldCoin]
resolveTxOutGold x = go x []
  where
    go :: [Core.TxOut] -> [Core.GoldCoin] -> [Core.GoldCoin]
    go [] coins = coins
    go xs coins = case List.head xs of
      Core.TxOutGold _ coin -> go (List.tail xs) ([coin] <> coins)
      _                     -> go (List.tail xs) coins

resolveTxOutDollar :: [Core.TxOut] -> [Core.GoldDollar]
resolveTxOutDollar x = go x []
  where
    go :: [Core.TxOut] -> [Core.GoldDollar] -> [Core.GoldDollar]
    go [] coins = coins
    go xs coins = case List.head xs of
      Core.TxOutDollar _ coin -> go (List.tail xs) ([coin] <> coins)
      _                       -> go (List.tail xs) coins

txOuts :: Core.Tx -> Core.Utxo
txOuts tx = Map.fromList $ map (toTxInOut (hash tx)) (outs tx)

txIns :: Core.TxAux -> Set Core.TxIn
txIns = Set.fromList . Core._txUtxoInputs . Core.taTx

txAuxId :: Core.TxAux -> Core.TxId
txAuxId = hash . Core.taTx

{-------------------------------------------------------------------------------
  External auxiliary
-------------------------------------------------------------------------------}

sumCoinsUnsafe :: (Container coins, Element coins ~ Core.Coin)
               => coins -> Core.Coin
sumCoinsUnsafe = unsafeIntegerToCoin . sumCoins

sumGoldCoinsUnsafe :: (Container gcoins, Element gcoins ~ Core.GoldCoin)
                   => gcoins -> Core.GoldCoin
sumGoldCoinsUnsafe = unsafeIntegerToGoldCoin . sumGoldCoins

sumGoldDollarsUnsafe :: (Container dcoins, Element dcoins ~ Core.GoldDollar)
                   => dcoins -> Core.GoldDollar
sumGoldDollarsUnsafe = unsafeIntegerToGoldDollar . sumGoldDollars

sumCoinGroupsUnsafe :: [Core.CoinGroup] -> Core.CoinGroup
sumCoinGroupsUnsafe cgs = Core.CoinGroup cs gs ds
  where
    cs = sumCoinsUnsafe $ map Core.cgSeal cgs
    gs = sumGoldCoinsUnsafe $ map Core.cgGold cgs
    ds = sumGoldDollarsUnsafe $ map Core.cgDollar cgs

-- | This is not unsafe although we use unsafeGetCoin, because
-- this is not actually unsafe either.
absCoin :: Core.Coin -> Core.Coin -> Core.Coin
absCoin ca cb
    | a >= b = Core.Coin (a-b)
    | otherwise = Core.Coin (b-a)
    where
      a = unsafeGetCoin ca
      b = unsafeGetCoin cb

absGoldCoin :: Core.GoldCoin -> Core.GoldCoin -> Core.GoldCoin
absGoldCoin ca cb
    | a >= b = Core.GoldCoin (a-b)
    | otherwise = Core.GoldCoin (b-a)
    where
      a = unsafeGetGoldCoin ca
      b = unsafeGetGoldCoin cb

absGoldDollar :: Core.GoldDollar -> Core.GoldDollar -> Core.GoldDollar
absGoldDollar ca cb
    | a >= b = Core.GoldDollar (a-b)
    | otherwise = Core.GoldDollar (b-a)
    where
      a = unsafeGetGoldDollar ca
      b = unsafeGetGoldDollar cb

absCoinGroup :: Core.CoinGroup -> Core.CoinGroup -> Core.CoinGroup
absCoinGroup ca cb = Core.CoinGroup seal gold dollar 
    where 
      seal = absCoin (Core.cgSeal ca) (Core.cgSeal cb)
      gold = absGoldCoin (Core.cgGold ca) (Core.cgGold cb)
      dollar = absGoldDollar (Core.cgDollar ca) (Core.cgDollar cb)

-- | Gets the underlying value (as a 'Coin') from a 'TxOutAux'.
toCoin :: Core.TxOutAux -> Core.Coin
toCoin = toc . Core.toaOut
  where
    toc :: Core.TxOut -> Core.Coin
    toc Core.TxOutSeal{..} = txOutSeal
    toc _                  = Core.Coin 0

toGoldCoin :: Core.TxOutAux -> Core.GoldCoin
toGoldCoin = toc . Core.toaOut
  where
    toc :: Core.TxOut -> Core.GoldCoin
    toc Core.TxOutGold{..} = txOutGold
    toc _                  = Core.GoldCoin 0

toGoldDollar :: Core.TxOutAux -> Core.GoldDollar
toGoldDollar = toc . Core.toaOut
  where
    toc :: Core.TxOut -> Core.GoldDollar
    toc Core.TxOutDollar{..} = txOutDollar
    toc _                  = Core.GoldDollar 0


{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Gets the underlying address from a 'TxOutAux'.
toAddress :: Core.TxOutAux -> Core.Address
toAddress = Core.txOutAddress . Core.toaOut

outs :: Core.Tx -> [(Word32, Core.TxOut)]
outs tx = enumerate $ Core._txUtxoOutputs tx

toTxInOut :: Core.TxId -> (Word32, Core.TxOut) -> (Core.TxIn, Core.TxOutAux)
toTxInOut txId (idx, out) = (Core.TxInUtxo txId idx, Core.TxOutAux out)

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ViewPatterns     #-}

module Seal.Wallet.Core
       ( checkCoin
       , checkGoldCoin
       , checkGoldDollar

       , mkCoin
       , mkGoldCoin
       , mkGoldDollar

       , unsafeAddCoin
       , unsafeAddGoldCoin
       , unsafeAddGoldDollar
       , unsafeAddCoinGroup

       , addCoin
       , addGoldCoin
       , addGoldDollar

       , coinF
       , goldCoinF
       , goldDollarF
       , sumCoins
       , sumGoldCoins
       , sumGoldDollars

       , unsafeSubCoin
       , unsafeSubGoldCoin
       , unsafeSubGoldDollar
       , unsafeSubCoinGroup

       , mulCoin
       , mulGoldCoin
       , mulGoldDollar

       , unsafeMulCoin
       , unsafeMulGoldCoin
       , unsafeMulGoldDollar

       , divCoin
       , divGoldCoin
       , divGoldDollar
       , leftToPanic

       , unsafeIntegerToCoin
       , unsafeIntegerToGoldCoin
       , unsafeIntegerToGoldDollar

       , integerToCoin
       , integerToGoldCoin
       , integerToGoldDollar

       , subCoin
       , subGoldCoin
       , subGoldDollar

       , unsafeGetCoin
       , unsafeGetGoldCoin
       , unsafeGetGoldDollar

       , coinToInteger
       , goldCoinToInteger
       , goldDollarToInteger

       , toSqlSlot
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Formatting (Format (..), build)
import           Seal.Core (Coin (..), GoldCoin (..), GoldDollar (..), 
                         CoinGroup (..), maxCoinVal, maxGoldCoinVal, 
                         maxGoldDollarVal, SlotId (..), EpochIndex (..),
                         LocalSlotIndex (..))
import           Seal.Util.Json.Canonical ()


-- | Makes a 'Coin' but is _|_ if that coin exceeds 'maxCoinVal'.
-- You can also use 'checkCoin' to do that check.
checkCoin :: MonadError Text m => Coin -> m ()
checkCoin (Coin c)
    | c <= maxCoinVal = pure ()
    | otherwise       = throwError $ "Coin: " <> show c <> " is too large"

checkGoldCoin :: MonadError Text m => GoldCoin -> m ()
checkGoldCoin (GoldCoin c)
    | c <= maxGoldCoinVal = pure ()
    | otherwise           = throwError $ "GoldCoin: " <> show c <> " is too large"

checkGoldDollar :: MonadError Text m => GoldDollar -> m ()
checkGoldDollar (GoldDollar c)
    | c <= maxGoldDollarVal = pure ()
    | otherwise       = throwError $ "GoldDollar: " <> show c <> " is too large"    

mkCoin :: Word64 -> Coin
mkCoin c = either error (const coin) (checkCoin coin)
  where
    coin = (Coin c)

mkGoldCoin :: Word64 -> GoldCoin
mkGoldCoin c = either error (const goldcoin) (checkGoldCoin goldcoin)
  where
    goldcoin = (GoldCoin c)

mkGoldDollar :: Word64 -> GoldDollar
mkGoldDollar c = either error (const golddollar) (checkGoldDollar golddollar)
  where
    golddollar = (GoldDollar c)

addCoin :: Coin -> Coin -> Maybe Coin
addCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | res >= a && res >= b && res <= unsafeGetCoin (maxBound @Coin) = Just (Coin res)
    | otherwise = Nothing
  where
    res = a+b      

addGoldCoin :: GoldCoin -> GoldCoin -> Maybe GoldCoin
addGoldCoin (unsafeGetGoldCoin -> a) (unsafeGetGoldCoin -> b)
    | res >= a && res >= b && res <= unsafeGetGoldCoin (maxBound @GoldCoin) = Just (GoldCoin res)
    | otherwise = Nothing
  where
    res = a+b   

addGoldDollar :: GoldDollar -> GoldDollar -> Maybe GoldDollar
addGoldDollar (unsafeGetGoldDollar -> a) (unsafeGetGoldDollar -> b)
    | res >= a && res >= b && res <= unsafeGetGoldDollar (maxBound @GoldDollar) = Just (GoldDollar res)
    | otherwise = Nothing
  where
    res = a+b   

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

goldCoinF :: Format r (GoldCoin -> r)
goldCoinF = build

goldDollarF :: Format r (GoldDollar -> r)
goldDollarF = build

-- | Compute sum of all coins in container. Result is 'Integer' as a
-- protection against possible overflow. If you are sure overflow is
-- impossible, you can use 'unsafeIntegerToCoin'.
sumCoins
    :: (Container coins, Element coins ~ Coin)
    => coins -> Integer
sumCoins = sum . map coinToInteger . toList

sumGoldCoins
    :: (Container gcoins, Element gcoins ~ GoldCoin)
    => gcoins -> Integer
sumGoldCoins = sum . map goldCoinToInteger . toList

sumGoldDollars
    :: (Container dcoins, Element dcoins ~ GoldDollar)
    => dcoins -> Integer
sumGoldDollars = sum . map goldDollarToInteger . toList

-- | Only use if you're sure there'll be no underflow.
unsafeSubCoin :: Coin -> Coin -> Coin
unsafeSubCoin a b = fromMaybe (error "unsafeSubCoin: underflow") (subCoin a b)

unsafeSubGoldCoin :: GoldCoin -> GoldCoin -> GoldCoin
unsafeSubGoldCoin a b = fromMaybe (error "unsafeSubGoldCoin: underflow") (subGoldCoin a b)

unsafeSubGoldDollar :: GoldDollar -> GoldDollar -> GoldDollar
unsafeSubGoldDollar a b = fromMaybe (error "unsafeSubGoldDollar: underflow") (subGoldDollar a b)

unsafeSubCoinGroup :: CoinGroup -> CoinGroup -> CoinGroup
unsafeSubCoinGroup (CoinGroup c g d) (CoinGroup c' g' d') = 
    CoinGroup (unsafeSubCoin c c')
              (unsafeSubGoldCoin g g')
              (unsafeSubGoldDollar d d')


-- | Multiplication between 'Coin's. Returns 'Nothing' in case of overflow.
mulCoin :: Integral a => Coin -> a -> Maybe Coin
mulCoin (unsafeGetCoin -> a) b
    | res <= coinToInteger (maxBound @Coin) = Just $ Coin (fromInteger res)
    | otherwise = Nothing
  where
    res = toInteger a * toInteger b

mulGoldCoin :: Integral a => GoldCoin -> a -> Maybe GoldCoin
mulGoldCoin (unsafeGetGoldCoin -> a) b
    | res <= goldCoinToInteger (maxBound @GoldCoin) = Just $ GoldCoin (fromInteger res)
    | otherwise = Nothing
  where
    res = toInteger a * toInteger b

mulGoldDollar :: Integral a => GoldDollar -> a -> Maybe GoldDollar
mulGoldDollar (unsafeGetGoldDollar -> a) b
    | res <= goldDollarToInteger (maxBound @GoldDollar) = Just $ GoldDollar (fromInteger res)
    | otherwise = Nothing
  where
    res = toInteger a * toInteger b    

-- | Only use if you're sure there'll be no overflow.
unsafeMulCoin :: Integral a => Coin -> a -> Coin
unsafeMulCoin a b =
    case mulCoin a b of
         Just r  -> r
         Nothing -> error "unsafeMulCoin: overflow"

unsafeMulGoldCoin :: Integral a => GoldCoin -> a -> GoldCoin
unsafeMulGoldCoin a b =
    case mulGoldCoin a b of
         Just r  -> r
         Nothing -> error "unsafeMulGoldCoin: overflow"

unsafeMulGoldDollar :: Integral a => GoldDollar -> a -> GoldDollar
unsafeMulGoldDollar a b =
    case mulGoldDollar a b of
         Just r  -> r
         Nothing -> error "unsafeMulGoldDollar: overflow"

divCoin :: Integral b => Coin -> b -> Coin
divCoin (unsafeGetCoin -> a) b = Coin (a `div` fromIntegral b)

divGoldCoin :: Integral b => GoldCoin -> b -> GoldCoin
divGoldCoin (unsafeGetGoldCoin -> a) b = GoldCoin (a `div` fromIntegral b)

divGoldDollar :: Integral b => GoldDollar -> b -> GoldDollar
divGoldDollar (unsafeGetGoldDollar -> a) b = GoldDollar (a `div` fromIntegral b)

leftToPanic :: Buildable a => Text -> Either a b -> b
leftToPanic msgPrefix = either (error . mappend msgPrefix . pretty) identity

unsafeIntegerToCoin :: Integer -> Coin
unsafeIntegerToCoin n = leftToPanic "unsafeIntegerToCoin: " (integerToCoin n)

unsafeIntegerToGoldCoin :: Integer -> GoldCoin
unsafeIntegerToGoldCoin n = leftToPanic "unsafeIntegerToGoldCoin: " (integerToGoldCoin n)

unsafeIntegerToGoldDollar :: Integer -> GoldDollar
unsafeIntegerToGoldDollar n = leftToPanic "unsafeIntegerToGoldDollar: " (integerToGoldDollar n)

integerToCoin :: Integer -> Either Text Coin
integerToCoin n
    | n < 0 = Left $ "integerToCoin: value is negative (" <> show n <> ")"
    | n <= coinToInteger (maxBound :: Coin) = pure $ Coin (fromInteger n)
    | otherwise = Left $ "integerToCoin: value is too big (" <> show n <> ")"

integerToGoldCoin :: Integer -> Either Text GoldCoin
integerToGoldCoin n
    | n < 0 = Left $ "integerToGoldCoin: value is negative (" <> show n <> ")"
    | n <= goldCoinToInteger (maxBound :: GoldCoin) = pure $ GoldCoin (fromInteger n)
    | otherwise = Left $ "integerToGoldCoin: value is too big (" <> show n <> ")"

integerToGoldDollar :: Integer -> Either Text GoldDollar
integerToGoldDollar n
    | n < 0 = Left $ "integerToGoldDollar: value is negative (" <> show n <> ")"
    | n <= goldDollarToInteger (maxBound :: GoldDollar) = pure $ GoldDollar (fromInteger n)
    | otherwise = Left $ "integerToGoldDollar: value is too big (" <> show n <> ")"

subCoin :: Coin -> Coin -> Maybe Coin
subCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | a >= b = Just (Coin (a-b))
    | otherwise = Nothing

subGoldCoin :: GoldCoin -> GoldCoin -> Maybe GoldCoin
subGoldCoin (unsafeGetGoldCoin -> a) (unsafeGetGoldCoin -> b)
    | a >= b = Just (GoldCoin (a-b))
    | otherwise = Nothing

subGoldDollar :: GoldDollar -> GoldDollar -> Maybe GoldDollar
subGoldDollar (unsafeGetGoldDollar -> a) (unsafeGetGoldDollar -> b)
    | a >= b = Just (GoldDollar (a-b))
    | otherwise = Nothing

unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
    
unsafeGetGoldCoin :: GoldCoin -> Word64
unsafeGetGoldCoin = getGoldCoin

unsafeGetGoldDollar :: GoldDollar -> Word64
unsafeGetGoldDollar = getGoldDollar

unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin a b =
    case addCoin a b of
        Just r -> r
        Nothing ->
            error $ "unsafeAddCoin: overflow when summing " <> show a <> " + " <> show b

unsafeAddGoldCoin :: GoldCoin -> GoldCoin -> GoldCoin
unsafeAddGoldCoin a b =
    case addGoldCoin a b of
        Just r -> r
        Nothing ->
            error $ "unsafeAddGoldCoin: overflow when summing " <> show a <> " + " <> show b

unsafeAddGoldDollar :: GoldDollar -> GoldDollar -> GoldDollar
unsafeAddGoldDollar a b =
    case addGoldDollar a b of
        Just r -> r
        Nothing ->
            error $ "unsafeAddGoldDollar: overflow when summing " <> show a <> " + " <> show b 

unsafeAddCoinGroup :: CoinGroup -> CoinGroup -> CoinGroup
unsafeAddCoinGroup (CoinGroup c g d) (CoinGroup c' g' d') =
    CoinGroup (unsafeAddCoin c c') (unsafeAddGoldCoin g g') (unsafeAddGoldDollar d d')


coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unsafeGetCoin

goldCoinToInteger :: GoldCoin -> Integer
goldCoinToInteger = toInteger . unsafeGetGoldCoin

goldDollarToInteger :: GoldDollar -> Integer
goldDollarToInteger = toInteger . unsafeGetGoldDollar

toSqlSlot :: SlotId -> Text
toSqlSlot slot = epoch <> "@" <> slotIndex
    where
        epoch = show $ getEpochIndex $ siEpoch slot
        slotIndex = show $ getSlotIndex $ siSlot slot
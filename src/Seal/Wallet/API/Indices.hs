{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Seal.Wallet.API.Indices (
    module Seal.Wallet.API.Indices
    -- * Re-exports from IxSet for convenience
    -- (these were previously /defined/ in this module)
  , IndicesOf
  , IxSet
  , Indexable
  , IsIndexOf
  ) where

import           Universum

import           Control.Monad.Except (MonadError, throwError)
import qualified Data.Text as T
import           GHC.TypeLits
import qualified Seal.Core as Core
import           Seal.Crypto (decodeHash)
import           Seal.Wallet.API.V1.Types

import           Seal.Wallet.Kernel.DB.Util.IxSet (HasPrimKey (..),
                     Indexable, IndicesOf, IsIndexOf, IxSet, OrdByPrimKey,
                     ixFun, ixList)
import qualified Data.IxSet.Typed as IxSet

-- | 'ToIndex' represents the witness that we can build an index 'ix' for a resource 'a'
-- from an input 'Text'.
class ToIndex a ix where
    -- | How to build this index from the input 'Text'.
    toIndex  :: Proxy a -> Text -> Maybe ix
    -- | How to access this index from the input data.
    accessIx :: (a -> ix)

instance ToIndex Wallet WalletId where
    toIndex _ x = Just (WalletId x)
    accessIx Wallet{..} = walId

instance ToIndex EosWallet WalletId where
    toIndex _ = toIndex (Proxy @Wallet)
    accessIx EosWallet{..} = eoswalId

instance ToIndex Wallet Core.Coin where
    toIndex _ x = case readMaybe (T.unpack x) of
        Nothing                       -> Nothing
        Just c  | c > Core.maxCoinVal -> Nothing
        Just c                        -> Just (mkCoin c)
    accessIx Wallet{..} = let (WalletCoin balance) = walBalance in balance

checkCoin :: MonadError Text m => Core.Coin -> m ()
checkCoin (Core.Coin c)
    | c <= Core.maxCoinVal = pure ()
    | otherwise       = throwError $ "Coin: " <> show c <> " is too large"

mkCoin :: Word64 -> Core.Coin
mkCoin c = either error (const coin) (checkCoin coin)
  where
    coin = (Core.Coin c)

instance ToIndex EosWallet Core.Coin where
    toIndex _ = toIndex (Proxy @Wallet)
    accessIx EosWallet{..} = let (WalletCoin balance) = eoswalBalance in balance

instance ToIndex Wallet WalletTimestamp where
    toIndex _ = fmap WalletTimestamp . Core.parseTimestamp
    accessIx = walCreatedAt

instance ToIndex EosWallet WalletTimestamp where
    toIndex _ = toIndex (Proxy @Wallet)
    accessIx  = eoswalCreatedAt

instance ToIndex Transaction WalletTxId where
    toIndex _ = fmap WalletTxId . rightToMaybe . decodeHash
    accessIx Transaction{..} = txId

instance ToIndex Transaction WalletTimestamp where
    toIndex _ = fmap WalletTimestamp . Core.parseTimestamp
    accessIx Transaction{..} = txCreationTime

instance ToIndex WalletAddress WalAddress where
    toIndex _ = fmap WalAddress . either (const Nothing) Just . Core.decodeTextAddress
    accessIx WalletAddress{..} = addrId

--
-- Primary and secondary indices for V1 types
--

instance HasPrimKey Wallet where
    type PrimKey Wallet = WalletId
    primKey = walId

instance HasPrimKey EosWallet where
    type PrimKey EosWallet = WalletId
    primKey = eoswalId

instance HasPrimKey Account where
    type PrimKey Account = AccountIndex
    primKey = accIndex

instance HasPrimKey Transaction where
    type PrimKey Transaction = WalletTxId
    primKey = txId

instance HasPrimKey WalletAddress where
    type PrimKey WalletAddress = WalAddress
    primKey = addrId

-- | The secondary indices for each major resource.
type SecondaryWalletIxs        = '[Core.Coin, WalletTimestamp]
type SecondaryTransactionIxs   = '[WalletTimestamp]
type SecondaryAccountIxs       = '[]
type SecondaryWalletAddressIxs = '[]

type instance IndicesOf EosWallet     = SecondaryWalletIxs
type instance IndicesOf Wallet        = SecondaryWalletIxs
type instance IndicesOf Account       = SecondaryAccountIxs
type instance IndicesOf Transaction   = SecondaryTransactionIxs
type instance IndicesOf WalletAddress = SecondaryWalletAddressIxs

--
-- Indexable instances for V1 types
--
-- TODO [CBR-356] These should not exist! We should not create 'IxSet's
-- (with their indices) on the fly. Fortunately, the only one for which this
-- is /really/ important is addresses, for which we already have special
-- cases. Nonetheless, the instances below should also go.
--
-- Instance for 'WalletAddress' is available only in
-- "Seal.Wallet.API.V1.LegacyHandlers.Instances". The same should be done
-- for the other instances here.
--

instance IxSet.Indexable (WalletId ': SecondaryWalletIxs)
                         (OrdByPrimKey Wallet) where
    indices = ixList (ixFun ((:[]) . unWalletCoin . walBalance))
                     (ixFun ((:[]) . walCreatedAt))

instance IxSet.Indexable (WalletId ': SecondaryWalletIxs)
                         (OrdByPrimKey EosWallet) where
    indices = ixList (ixFun ((:[]) . unWalletCoin . eoswalBalance))
                     (ixFun ((:[]) . eoswalCreatedAt))

instance IxSet.Indexable (WalletTxId ': SecondaryTransactionIxs)
                         (OrdByPrimKey Transaction) where
    indices = ixList (ixFun (\Transaction{..} -> [txCreationTime]))

instance IxSet.Indexable (AccountIndex ': SecondaryAccountIxs)
                         (OrdByPrimKey Account) where
    indices = ixList

-- | Extract the parameter names from a type leve list with the shape
type family ParamNames res xs where
    ParamNames res '[] =
        '[]
    ParamNames res (ty ': xs) =
        IndexToQueryParam res ty ': ParamNames res xs

-- | This type family allows you to recover the query parameter if you know
-- the resource and index into that resource.
type family IndexToQueryParam resource ix where
    IndexToQueryParam Account AccountIndex            = "id"

    IndexToQueryParam Wallet  Core.Coin               = "balance"
    IndexToQueryParam Wallet  WalletId                = "id"
    IndexToQueryParam Wallet  WalletTimestamp         = "created_at"

    IndexToQueryParam EosWallet  Core.Coin            = "balance"
    IndexToQueryParam EosWallet  WalletId             = "id"
    IndexToQueryParam EosWallet  WalletTimestamp      = "created_at"

    IndexToQueryParam WalletAddress (WalAddress)      = "address"

    IndexToQueryParam Transaction WalletTxId         = "id"
    IndexToQueryParam Transaction WalletTimestamp    = "created_at"

    -- This is the fallback case. It will trigger a type error if you use
    -- 'IndexToQueryParam'' with a pairing that is invalid. We want this to
    -- trigger early, so that we don't get Weird Errors later on with stuck
    -- types.
    IndexToQueryParam res ix = TypeError (
        'Text "You used `IndexToQueryParam' with the following resource:"
        ':$$: 'Text "    " ':<>: 'ShowType res
        ':$$: 'Text "and index type:"
        ':$$: 'Text "    " ':<>: 'ShowType ix
        ':$$: 'Text "But no instance for that type was defined."
        ':$$: 'Text "Perhaps you mismatched a resource and an index?"
        ':$$: 'Text "Or, maybe you need to add a type instance to `IndexToQueryParam'."
        )

-- | Type-level composition of 'KnownSymbol' and 'IndexToQueryParam'
--
-- TODO: Alternatively, it would be possible to get rid of 'IndexToQueryParam'
-- completely and just have the 'KnownQueryParam' class.
class KnownSymbol (IndexToQueryParam resource ix) => KnownQueryParam resource ix

instance KnownQueryParam Account AccountIndex
instance KnownQueryParam Wallet Core.Coin
instance KnownQueryParam Wallet WalletId
instance KnownQueryParam Wallet WalletTimestamp
instance KnownQueryParam EosWallet Core.Coin
instance KnownQueryParam EosWallet WalletId
instance KnownQueryParam EosWallet WalletTimestamp
instance KnownQueryParam WalletAddress WalAddress
instance KnownQueryParam Transaction WalletTxId
instance KnownQueryParam Transaction WalletTimestamp

{-# LANGUAGE MultiWayIf #-}

-- | Read-only access to the DB
module Seal.Wallet.Kernel.Read (
    -- * Read-only access to the DB
    DB -- opaque
    -- ** HdRnd Helper
  , getFOWallets
    -- ** Eos Helper
  , getEosPools
  , addressPoolGapByRootId
  , getWalletOwnership
  , getWalletsByOwnership
  , eosAccountsByRootId
  , WalletOwnership (..)
    -- Errors
  , GetAddressPoolGapError (..)
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- ** Pure getters acting on a DB snapshot
  , module Getters
  ) where

import           Universum hiding (State)

import           Data.Acid.Advanced (query')
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Formatting (bprint, build, sformat, (%))
import qualified Formatting.Buildable
import           Serokell.Util (listJson)

import           Seal.Core (Address)
import           Seal.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Seal.Crypto (EncryptedSecretKey, ProtocolMagic, PublicKey)
import           Seal.Util.Wlog (Severity (..))

import           Seal.Wallet.Kernel.AddressPool (AddressPool)
import           Seal.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Seal.Wallet.Kernel.DB.AcidState (DB, Snapshot (..),
                     dbHdWallets)
import           Seal.Wallet.Kernel.DB.HdRootId (HdRootId)
import           Seal.Wallet.Kernel.DB.HdWallet (HdAccountBase (..),
                     HdAccountId, HdAddress, HdRoot, getHdAddressIx,
                     hdAccountBase, hdAddressAddress, hdAddressId,
                     hdAddressIdIx, hdRootId, hdWalletsRoots,
                     mkAddressPoolExisting)
import           Seal.Wallet.Kernel.DB.InDb (fromDb)
import           Seal.Wallet.Kernel.DB.Read as Getters
import           Seal.Wallet.Kernel.DB.Util.IxSet (Indexed (..))
import qualified Seal.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Seal.Wallet.Kernel.Internal
import qualified Seal.Wallet.Kernel.Keystore as Keystore

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot

{-------------------------------------------------------------------------------
    Get Prefiltering context for all HdRnd wallets
-------------------------------------------------------------------------------}

-- | Prefiltering Context for HdRnd wallets
getFOWallets
    :: PassiveWallet
    -> DB
    -> IO (Map HdRootId EncryptedSecretKey)
getFOWallets pw db
    = getWalletCredentials db
        (pw ^. walletKeystore)
        (pw ^. walletProtocolMagic)
        (pw ^. walletLogMessage)

-- | Get wallet credentials
--
-- For wallets without a corresponding secret key we log an error. This
-- indicates a bug somewhere, but there is not much we can do about it here,
-- since this runs in the context of applying a block.
getWalletCredentials
    :: DB
    -> Keystore.Keystore
    -> ProtocolMagic
    -> (Severity -> Text -> IO ())
    -> IO (Map HdRootId EncryptedSecretKey)
getWalletCredentials snapshot ks pm logger = do
    (creds, missing) <- fmap partitionEithers $
      forM (walletIds snapshot) $ \walletId ->
        aux walletId <$> Keystore.lookup nm walletId ks
    unless (null missing) $ logger Error (errMissing missing)
    return (Map.fromList creds)
  where
    nm :: NetworkMagic
    nm = makeNetworkMagic pm

    aux :: HdRootId
        -> Maybe EncryptedSecretKey
        -> Either (HdRootId, EncryptedSecretKey) HdRootId
    aux walletId Nothing    = Right walletId
    aux walletId (Just esk) = Left (walletId, esk)

    errMissing :: [HdRootId] -> Text
    errMissing = sformat ("Root key missing for " % listJson)

{-------------------------------------------------------------------------------
    Get Prefiltering context for all EOS wallets
-------------------------------------------------------------------------------}

-- | Gathers all Eo accounts in all Eo roots and builds an AddressPool
-- for each Eo account by using the existing addresses for the account.
--
-- NOTE: Fo wallet accounts are ignored.
getEosPools
    :: MonadIO m
    => DB
    -> (PublicKey -> Address)
    -> m (Map HdAccountId (AddressPool Address))
getEosPools db mkAddress
    = return . Map.fromList $ concatMap toAccountAddressPools' allRoots
  where
    allRoots = IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots

    toAccountAddressPools' :: HdRoot -> [(HdAccountId, AddressPool Address)]
    toAccountAddressPools' root
        = toAccountAddressPools root (eosAccountsByRootId (root ^. hdRootId) db)

    toAccountAddressPools
        :: HdRoot
        -> Maybe (Either GetAddressPoolGapError ([(HdAccountId, PublicKey)], AddressPoolGap))
        -> [(HdAccountId, AddressPool Address)]
    toAccountAddressPools _root accs
        = case accs of
            Nothing                    -> [] -- not an Eos root
            (Just (Left err))          -> error (sformat build err)
            (Just (Right (accs_,gap))) -> map (mkPool gap) accs_

    mkPool
        :: AddressPoolGap
        -> (HdAccountId, PublicKey)
        -> (HdAccountId, AddressPool Address)
    mkPool gap (accId,pk)
        = case mkAddressPoolExisting mkAddress pk gap (getAddrs accId) of
            Left invalidPoolErr -> error (sformat build invalidPoolErr)
            Right pool          -> (accId, pool)

    getAddrs :: HdAccountId -> [(Address, Word32)]
    getAddrs accId
        = map (toAddr . _ixedIndexed) $
            IxSet.toList (Getters.addressesByAccountId db accId)

    toAddr :: HdAddress -> (Address, Word32)
    toAddr a = ( a ^. hdAddressAddress . fromDb
               , getHdAddressIx (a ^. hdAddressId . hdAddressIdIx))


-- | For a given rootId, returns either Nothing if this is not an Eo wallet,
-- or Just the address pool gap along with the account ids and public keys.
--
-- For both Eo and Fo wallets we return an exception if there are
-- no accounts at all or if the accounts are not all of the same type.
-- For Eo wallets we also return an exception if the gap is not consistent
-- for all accounts in the wallet.
eosAccountsByRootId
    :: HdRootId
    -> DB
    -> Maybe (Either GetAddressPoolGapError ([(HdAccountId, PublicKey)], AddressPoolGap))
eosAccountsByRootId rootId db = do
    let accounts = IxSet.toList $ Getters.accountsByRootId db rootId
        bases = flip map accounts $ \hdAccount -> case hdAccount ^. hdAccountBase of
                    -- It is EOS-wallet, so all accounts must have EO-branch.
                    HdAccountBaseFO _ -> Left ()
                    HdAccountBaseEO accId accPk gap -> Right (accId,accPk,gap)
        (accFOs, accEOs) = partitionEithers bases
        gaps = map (\(_,_,gap) -> gap) accEOs
        accs' = map (\(accId,accPk,_) -> (accId,accPk)) accEOs
    if | null bases ->
            Just . Left $ GetEosWalletErrorNoAccounts anId
       | mixedAccs accFOs accEOs ->
            Just . Left $ GetEosWalletErrorWrongAccounts anId
       | onlyA accFOs accEOs ->
            Nothing -- this is not an EO wallet
       | gapsDiffer gaps ->
            Just . Left $ GetEosWalletErrorGapsDiffer anId
       | otherwise ->
            let gap:_ = gaps in (Just . Right) (accs',gap)
  where
    anId = sformat build rootId

    mixedAccs a b = not $ (onlyA a b) || (onlyA b a)
    onlyA a b = ((not . null) a) && (null b)
    gapsDiffer gs = length (nub gs) > 1

addressPoolGapByRootId
    :: HdRootId
    -> DB
    -> Either GetAddressPoolGapError AddressPoolGap
addressPoolGapByRootId rootId db
    = case eosAccountsByRootId rootId db of
        Nothing -> -- not an EO wallet
            Left $ GetEosWalletErrorWrongAccounts (sformat build rootId)
        Just res -> snd <$> res

getWalletOwnership
    :: HdRootId
    -> DB
    -> WalletOwnership
getWalletOwnership rootId db =
    case addressPoolGapByRootId rootId db of
        Right _ -> WalletExternallyOwned
        Left _  -> WalletFullyOwned

getWalletsByOwnership
    :: WalletOwnership
    -> DB
    -> [HdRoot]
getWalletsByOwnership ownership db =
    flip filter allRoots $ \root ->
        getWalletOwnership (root ^. hdRootId) db == ownership
  where
    allRoots = IxSet.toList $ db ^. dbHdWallets . hdWalletsRoots

data GetAddressPoolGapError =
      GetEosWalletErrorNoAccounts Text
    | GetEosWalletErrorWrongAccounts Text
    | GetEosWalletErrorGapsDiffer Text
    deriving Eq

instance Buildable GetAddressPoolGapError where
    build (GetEosWalletErrorNoAccounts txt) =
        bprint ("GetEosWalletErrorNoAccounts " % build) txt
    build (GetEosWalletErrorWrongAccounts txt) =
        bprint ("FO-accounts found in EOS-wallet " % build) txt
    build (GetEosWalletErrorGapsDiffer txt) =
        bprint ("Address pool gaps differ, for EOS-wallet " % build) txt

data WalletOwnership
    = WalletFullyOwned
    | WalletExternallyOwned
    deriving Eq

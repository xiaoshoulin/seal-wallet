module Seal.Wallet.Kernel.Wallets (
      createHdWallet
    , createEosHdWallet
    , mkEosAddress
    , updateHdWallet
    , updatePassword
    , deleteHdWallet
    , deleteEosHdWallet
    , defaultHdAccountId
    , defaultHdAddressId
    , defaultHdAddress
      -- * Errors
    , CreateWalletError(..)
    , GetAddressPoolGapError (..)
    , UpdateWalletPasswordError(..)
    -- * Internal & testing use only
    , createWalletHdRnd
    ) where

import qualified Prelude
import           Universum

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Formatting.Buildable

import           Data.Acid.Advanced (update')

import           Seal.Chain.Txp (Utxo)
import           Seal.Core (Address, Timestamp, makePubKeyAddressBoot)
import           Seal.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Seal.Crypto (EncryptedSecretKey, PassPhrase, ProtocolMagic,
                     PublicKey, changeEncPassphrase, checkPassMatches,
                     emptyPassphrase, firstHardened, safeDeterministicKeyGen)

import           Seal.Mnemonic (Mnemonic)
import qualified Seal.Mnemonic as Mnemonic
import           Seal.Wallet.Kernel.Addresses (newHdAddress)
import           Seal.Wallet.Kernel.AddressPoolGap (AddressPoolGap)
import           Seal.Wallet.Kernel.DB.AcidState (CreateHdWallet (..),
                     DeleteHdRoot (..), RestoreHdWallet,
                     UpdateHdRootPassword (..), UpdateHdWallet (..))
import           Seal.Wallet.Kernel.DB.HdRootId (HdRootId)
import qualified Seal.Wallet.Kernel.DB.HdRootId as HD
import           Seal.Wallet.Kernel.DB.HdWallet (AssuranceLevel,
                     HdAccountBase (..), HdAccountId (..), HdAccountIx (..),
                     HdAddress, HdAddressId (..), HdAddressIx (..), HdRoot,
                     WalletName, hdRootId)
import qualified Seal.Wallet.Kernel.DB.HdWallet as HD
import qualified Seal.Wallet.Kernel.DB.HdWallet.Create as HD
import           Seal.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Seal.Wallet.Kernel.Internal (PassiveWallet, walletKeystore,
                     walletProtocolMagic, wallets)
import qualified Seal.Wallet.Kernel.Keystore as Keystore
import           Seal.Wallet.Kernel.Read (GetAddressPoolGapError (..))
import qualified Seal.Wallet.Kernel.Read as Kernel
import           Seal.Wallet.Kernel.Util.Core (getCurrentTimestamp)

import           Test.QuickCheck (Arbitrary (..), oneof)

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data CreateWalletError =
      CreateWalletFailed HD.CreateHdRootError
      -- ^ When trying to create the 'Wallet', the DB operation failed.
    | CreateWalletDefaultAddressDerivationFailed
    -- ^ When generating the default address for the companion 'HdAddress',
    -- the derivation failed

instance Arbitrary CreateWalletError where
    arbitrary = oneof
        [ CreateWalletFailed . HD.CreateHdRootExists <$> arbitrary
        , pure $ CreateWalletFailed HD.CreateHdRootDefaultAddressDerivationFailed
        , pure $ CreateWalletDefaultAddressDerivationFailed
        ]

instance Buildable CreateWalletError where
    build (CreateWalletFailed dbOperation) =
        bprint ("CreateWalletUnknownHdAccount " % F.build) dbOperation
    build CreateWalletDefaultAddressDerivationFailed =
        bprint "CreateWalletDefaultAddressDerivationFailed"

instance Show CreateWalletError where
    show = formatToString build

data UpdateWalletPasswordError =
      UpdateWalletPasswordOldPasswordMismatch HD.HdRootId
      -- ^ When trying to update the wallet password, there was a mismatch
      -- with the old one.
    | UpdateWalletPasswordKeyNotFound HD.HdRootId
      -- ^ When trying to update the wallet password, there was no
      -- 'EncryptedSecretKey' in the Keystore for this 'HdRootId'.
    | UpdateWalletPasswordUnknownHdRoot HD.UnknownHdRoot
      -- ^ When trying to update the DB the input 'HdRootId' was not found.
    | UpdateWalletPasswordKeystoreChangedInTheMeantime HD.HdRootId
      -- ^ When trying to update the password inside the keystore, the
      -- previous 'PassPhrase' didn't match or it was deleted, which means
      -- this operation is not valid anymore.

instance Arbitrary UpdateWalletPasswordError where
    arbitrary = oneof
        [ UpdateWalletPasswordOldPasswordMismatch <$> arbitrary
        , UpdateWalletPasswordKeyNotFound <$> arbitrary
        , UpdateWalletPasswordUnknownHdRoot <$> arbitrary
        , UpdateWalletPasswordKeystoreChangedInTheMeantime <$> arbitrary
        ]

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordOldPasswordMismatch rootId) =
        bprint ("UpdateWalletPasswordOldPasswordMismatch " % F.build) rootId
    build (UpdateWalletPasswordKeyNotFound rootId) =
        bprint ("UpdateWalletPasswordKeyNotFound " % F.build) rootId
    build (UpdateWalletPasswordUnknownHdRoot uRoot) =
        bprint ("UpdateWalletPasswordUnknownHdRoot " % F.build) uRoot
    build (UpdateWalletPasswordKeystoreChangedInTheMeantime uRoot) =
        bprint ("UpdateWalletPasswordKeystoreChangedInTheMeantime " % F.build) uRoot

instance Show UpdateWalletPasswordError where
    show = formatToString build

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates a new HD 'Wallet'.
--
-- PRECONDITION: The input 'Mnemonic' should be supplied by the frontend such
-- that this is a brand new 'Mnemonic' never used before on the blockchain. For
-- other wallets restoration should be used.
createHdWallet :: PassiveWallet
               -> Mnemonic nat
               -- ^ The set of words (i.e the mnemonic) to generate the initial seed.
               -- See <https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#From_mnemonic_to_seed>
               -- This Kernel function is agnostic in the number of words, and it's
               -- wallet layer's responsibility to make sure that invalid sizes are
               -- rejected.
               -> PassPhrase
               -- ^ The spending password to encrypt the 'SecretKey' for the
               -- newly-generated wallet. If the user didn't specify any, the
               -- empty 'PassPhrase' is used.
               -> AssuranceLevel
               -- ^ The 'AssuranceLevel' for this wallet, namely after how many
               -- blocks each transaction is considered 'adopted'. This translates
               -- in the frontend with a different threshold for the confirmation
               -- range (@low@, @medium@, @high@).
               -> WalletName
               -- ^ The name for this wallet.
               -> IO (Either CreateWalletError HdRoot)
createHdWallet pw mnemonic spendingPassword assuranceLevel walletName = do
    -- STEP 1: Generate the 'EncryptedSecretKey' outside any acid-state
    -- transaction, to not leak it into acid-state's transaction logs.
    let (_, esk) = safeDeterministicKeyGen (Mnemonic.mnemonicToSeed mnemonic) spendingPassword

    -- STEP 2: Insert the key into the keystore. We do this preemptively so that,
    -- in case of asynchronous exceptions, the worst which can happen is for the
    -- key to stay dangling into the keystore and the wallet not to be created.
    -- Then calling 'createHdWallet' a second time in this situation would
    -- correctly persist the wallet in the DB.
    -- The converse won't be true: leaving a dangling 'HdRoot' without its associated
    -- key in the keystore would break the sytem consistency and make the wallet
    -- unusable.
    -- There is another important aspect about atomicity which deals with
    -- scheduling (as in thread scheduling, which means we could be interrupted
    -- at any time during our processing). In case we would create the wallet
    -- @before@ inserting the key, this wallet would now be available to 'applyBlock':
    -- but the latter would fail to fetch the relevant key from the keystore
    -- (We got interrupted before inserting it) causing a system panic.
    -- We can fix this properly as part of [CBR-404].
    let nm = makeNetworkMagic (pw ^. walletProtocolMagic)
        newRootId = HD.eskToHdRootId nm esk
    Keystore.insert newRootId esk (pw ^. walletKeystore)

    -- STEP 2.5: Generate the fresh Cardano Address which will be used for the
    -- companion 'HdAddress'
    let mbHdAddress =
            newHdAddress nm
                         esk
                         spendingPassword
                         (defaultHdAccountId newRootId)
                         (defaultHdAddressId newRootId)
    case mbHdAddress of
        Nothing -> do
            -- Here, ideally, we would like to do some cleanup and call
            -- >>> Keystore.delete newRootId (pw ^. walletKeystore)
            -- However, this wouldn't be correct in case, for example, the user
            -- is trying to create the same wallet twice. In that case we would
            -- wipe a pre-existing, perfectly valid key!
            -- The solution to this and other problems will be provided with [CBR-404].
            -- For the moment, the less evil solution is to simply allow dangling
            -- keys in the keystore. Being 'Keystore.insert' idempotent, doing so
            -- won't compromise using the wallet.
            return $ Left CreateWalletDefaultAddressDerivationFailed
        Just hdAddress -> do
            -- STEP 3: Atomically generate the wallet and the initial internal structure in
            -- an acid-state transaction.
            res <- createWalletHdRnd pw
                                     (spendingPassword /= emptyPassphrase)
                                     (Just (hdAddress ^. HD.hdAddressAddress . fromDb))
                                     walletName
                                     assuranceLevel
                                     esk
                                     -- Brand new wallets have no Utxo
                                     -- See preconditon above.
                                     (\hdRoot hdAccountId hdAddr ->
                                         Left $ CreateHdWallet hdRoot
                                            (Map.singleton (HdAccountBaseFO hdAccountId) (mempty, maybeToList hdAddr))
                                     )
            case res of
                 -- NOTE(adinapoli): This is the @only@ error the DB can return,
                 -- so we are pattern matching directly on it. In the
                 -- case of more errors being added, carefully thinking would have to
                 -- be put into whether or not we should remove the key from the keystore
                 -- at this point. Surely we don't want to do this in the case the
                 -- wallet already exists, or we would end up deleting the key of the
                 -- existing wallet!
                 -- Fix properly as part of [CBR-404].
                 Left e@(HD.CreateHdRootExists _) ->
                     return . Left $ CreateWalletFailed e
                 Left e@(HD.CreateHdRootDefaultAddressDerivationFailed) ->
                     return . Left $ CreateWalletFailed e

                 Right hdRoot -> return (Right hdRoot)

-- | Creates a new EOS HD 'Wallet'.
--
-- Here, we review the definition of a wallet down to a list of account public keys with
-- no relationship whatsoever from the wallet's point of view. New addresses can be derived
-- for each account at will and discovered using the address pool discovery algorithm
-- described in BIP-44. Public keys are managed and provided from an external sources.
createEosHdWallet :: PassiveWallet
                  -> NonEmpty (PublicKey, HdAccountIx)
                  -- ^ External wallets' accounts
                  -> AddressPoolGap
                  -- ^ Address pool gap for this wallet.
                  -> AssuranceLevel
                  -- ^ The 'AssuranceLevel' for this wallet, namely after how many
                  -- blocks each transaction is considered 'adopted'. This translates
                  -- in the frontend with a different threshold for the confirmation
                  -- range (@low@, @medium@, @high@).
                  -> WalletName
                  -- ^ The name for this wallet.
                  -> IO (Either CreateWalletError HdRoot)
createEosHdWallet pw accounts gap assuranceLevel walletName = do
    root <- initHdRoot' <$> HD.genHdRootId <*> getCurrentTimestamp
    let bases = Map.unionsWith (<>) (NE.toList (toAccountBase (root ^. hdRootId) <$> accounts))
    res <- update' (pw ^. wallets) $ CreateHdWallet root bases
    return $ case res of
        Left e@(HD.CreateHdRootExists _) ->
            Left $ CreateWalletFailed e
        Left e@(HD.CreateHdRootDefaultAddressDerivationFailed) ->
            Left $ CreateWalletFailed e
        Right _ ->
            Right root
  where
    initHdRoot' rootId created = HD.initHdRoot
        rootId
        walletName
        (HD.NoSpendingPassword (InDb created))
        assuranceLevel
        (InDb created)

    toAccountBase
        :: HdRootId
        -> (PublicKey, HdAccountIx)
        -> Map HdAccountBase (Utxo, [HdAddress])
    toAccountBase rootId =
        let
            accId ix = HdAccountId rootId ix
            mkBase (pk, ix) = HdAccountBaseEO (accId ix) pk gap
        in
            flip Map.singleton (mempty, mempty) . mkBase

mkEosAddress
    :: ProtocolMagic
    -> PublicKey
    -> Address
mkEosAddress pm
    = makePubKeyAddressBoot (makeNetworkMagic pm)

-- | Creates an HD wallet where new accounts and addresses are generated
-- via random index derivation.
--
-- Fails with CreateHdWalletError if the HdRootId already exists.
--
-- INVARIANT: Whenever we create an HdRoot, it @must@ come with a fresh
-- account at 'firstHardened' index.
--
-- We may also provide an OPTIONAL default address at 'firstHardened' index
-- (the default address is optional, since for that we need the spending password
-- which may not be available to the caller)
--
createWalletHdRnd :: PassiveWallet
                  -> Bool
                  -- Does this wallet have a spending password?
                  -> Maybe Address
                  -- Optional 'Address' to use for the companion 'HdAddress'.
                  -> HD.WalletName
                  -> AssuranceLevel
                  -> EncryptedSecretKey
                  -> (  HdRoot
                     -> HdAccountId
                     -> Maybe HdAddress
                     -> Either CreateHdWallet RestoreHdWallet
                     )
                  -> IO (Either HD.CreateHdRootError HdRoot)
createWalletHdRnd pw hasSpendingPassword defaultCardanoAddress name assuranceLevel esk createWallet = do
    created <- InDb <$> getCurrentTimestamp
    let nm      = makeNetworkMagic (pw ^. walletProtocolMagic)
        rootId  = HD.eskToHdRootId nm esk
        newRoot = HD.initHdRoot rootId
                                name
                                (hdSpendingPassword created)
                                assuranceLevel
                                created

        -- Atomically generate a new wallet with a default account & optional address
        doCreateOrRestore :: Maybe HdAddress -> IO (Either HD.CreateHdRootError HdRoot)
        doCreateOrRestore addr_ = do
            res <- case createWallet newRoot (defaultHdAccountId rootId) addr_ of
                Left  create  -> update' (pw ^. wallets) create
                Right restore -> update' (pw ^. wallets) restore
            return $ either Left (const (Right newRoot)) res

    -- Create or Restore the wallet (with or without a default address)
    case defaultCardanoAddress of
        Nothing -> -- no default addr given, that's ok, we can proceed without one
            doCreateOrRestore Nothing
        Just addr ->
            -- given a default Address, we attempt to derive an HdAddress, if
            -- this fails, we consider it an error.
            maybe (return $ Left HD.CreateHdRootDefaultAddressDerivationFailed)
                  (doCreateOrRestore . Just)
                  (defaultHdAddressWith esk rootId addr)
    where
        hdSpendingPassword :: InDb Timestamp -> HD.HasSpendingPassword
        hdSpendingPassword created =
            if hasSpendingPassword then HD.HasSpendingPassword created
                                   else HD.NoSpendingPassword created

-- | Creates a default 'HdAddress' at a fixed derivation path. This is
-- useful for tests, but otherwise you may want to use 'defaultHdAddressWith'.
defaultHdAddress :: NetworkMagic
                 -> EncryptedSecretKey
                 -> PassPhrase
                 -> HD.HdRootId
                 -> Maybe HdAddress
defaultHdAddress nm esk spendingPassword rootId =
    let hdAccountId = defaultHdAccountId rootId
        hdAddressId = HdAddressId hdAccountId (HdAddressIx firstHardened)
    in newHdAddress nm esk spendingPassword hdAccountId hdAddressId


-- | Given a Cardano 'Address', it returns a default 'HdAddress' at a fixed
-- and predictable generation path.
defaultHdAddressWith :: EncryptedSecretKey
                     -> HD.HdRootId
                     -> Address
                     -> Maybe HdAddress
defaultHdAddressWith esk rootId addr =
    fst $ HD.isOurs addr (Map.singleton rootId esk)

defaultHdAccountId :: HD.HdRootId -> HdAccountId
defaultHdAccountId rootId = HdAccountId rootId (HdAccountIx firstHardened)

defaultHdAddressId :: HD.HdRootId -> HdAddressId
defaultHdAddressId rootId =
    HdAddressId (defaultHdAccountId rootId) (HdAddressIx firstHardened)


deleteHdWallet :: NetworkMagic
               -> PassiveWallet
               -> HD.HdRootId
               -> IO (Either HD.UnknownHdRoot ())
deleteHdWallet nm wallet rootId = do
    -- STEP 1: Remove the HdRoot via an acid-state transaction which will
    --         also delete any associated accounts and addresses.
    res <- update' (wallet ^. wallets) $ DeleteHdRoot rootId
    case res of
        Left err -> return (Left err)
        Right () -> do
            -- STEP 2: Purge the key from the keystore.
            --
            -- NOTE ON ATOMICITY: In the case of asynchronous exceptions
            -- striking between STEP 1 & 2, note how this won't compromise the
            -- internal consistency of the system. Yes, it would leave a
            -- dangling key into the keystore, but that won't be as bad as
            -- trying to delete the key first @and then@ delete the wallet
            -- from the DB, which would expose us to consistency troubles as
            -- an 'HdRoot' without any associated keys in the keystore is
            -- unusable.
            -- Fix properly as part of [CBR-404].
            Keystore.delete nm rootId (wallet ^. walletKeystore)
            return $ Right ()

deleteEosHdWallet :: PassiveWallet
                  -> HD.HdRootId
                  -> IO (Either HD.UnknownHdRoot ())
deleteEosHdWallet wallet rootId = do
    -- STEP 1: Remove the HdRoot via an acid-state transaction which will
    --         also delete any associated accounts and addresses.
    res <- update' (wallet ^. wallets) $ DeleteHdRoot rootId
    case res of
        Left err -> return (Left err)
        Right () -> return (Right ())

{-------------------------------------------------------------------------------
  Wallet update
-------------------------------------------------------------------------------}

updateHdWallet :: PassiveWallet
               -> HD.HdRootId
               -> HD.AssuranceLevel
               -> HD.WalletName
               -> IO (Either HD.UnknownHdRoot (Kernel.DB, HdRoot))
updateHdWallet pw rootId assuranceLevel walletName = do
    res <- update' (pw ^. wallets) (UpdateHdWallet rootId assuranceLevel walletName)
    case res of
         Left e              -> return (Left e)
         Right (db, newRoot) -> return (Right (db, newRoot))

updatePassword :: PassiveWallet
               -> HD.HdRootId
               -> PassPhrase
               -- ^ The old 'PassPhrase' for this Wallet.
               -> PassPhrase
               -- ^ The new 'PassPhrase' for this Wallet.
               -> IO (Either UpdateWalletPasswordError (Kernel.DB, HdRoot))
updatePassword pw rootId oldPassword newPassword = do
    let nm       = makeNetworkMagic (pw ^. walletProtocolMagic)
        keystore = pw ^. walletKeystore
        wId      = rootId
    -- STEP 1: Lookup the key from the keystore
    mbKey <- Keystore.lookup nm wId keystore
    case mbKey of
         Nothing -> return $ Left $ UpdateWalletPasswordKeyNotFound rootId
         Just oldKey -> do

             -- Predicate to check that the 2 password matches. It gets passed
             -- down to the 'Keystore' to ensure atomicity.
             let pwdCheck = maybe False (const True) . checkPassMatches oldPassword

             -- STEP 2: Compute the new key using the cryptographic primitives
             --         we have. This operation doesn't change any state, it
             --         just recomputes the new 'EncryptedSecretKey' in-place.
             genNewKey <- changeEncPassphrase oldPassword newPassword oldKey
             let mbNewKey = maybeToRight (UpdateWalletPasswordOldPasswordMismatch rootId) $
                            genNewKey

             case mbNewKey of
                  Left e -> return (Left e)
                  Right newKey -> do
                      -- STEP 3: Update the keystore, atomically.
                      swapped <- Keystore.compareAndReplace nm wId pwdCheck newKey keystore
                      case swapped of
                           -- We failed, the password changed in the
                           -- meantime, and the user needs to repeat the
                           -- operation.
                           Keystore.PredicateFailed ->
                               return $ Left (UpdateWalletPasswordOldPasswordMismatch rootId)
                           -- We failed, in the meantime the user deleted the
                           -- key.
                           Keystore.OldKeyLookupFailed -> do
                               return $ Left (UpdateWalletPasswordKeystoreChangedInTheMeantime rootId)
                           Keystore.Replaced -> do
                               -- STEP 4: Update the timestamp in the wallet data storage.
                               -- If we get interrupted here by an asynchronous exception the
                               -- price we will pay would be a slightly incorrect notion of
                               -- "how long ago we did change the password", but it won't
                               -- compromise the integrity of the system. However, there is a
                               -- potentially subtle use case here: in case the user didn't set a
                               -- @spending password@ initially, but it does later in the lifecycle of
                               -- the wallet, this ghost update would be problematic, as the system would
                               -- report "HasNoSpendingPassword" instead of correctly reporting a password
                               -- was set.
                               -- Fix this properly as part of [CBR-404].
                               lastUpdateNow <- InDb <$> getCurrentTimestamp

                               -- There is no such thing as "removing" the spending password.
                               -- However, it can be set to the empty string. We check for that
                               -- here and update 'hasSpendingPassword' on 'HdRoot' accordingly.
                               let hasSpendingPassword =
                                       if newPassword == emptyPassphrase
                                       then HD.NoSpendingPassword lastUpdateNow
                                       else HD.HasSpendingPassword lastUpdateNow

                               res <- update' (pw ^. wallets)
                                              (UpdateHdRootPassword rootId hasSpendingPassword)
                               case res of
                                    Left e ->
                                        return $ Left (UpdateWalletPasswordUnknownHdRoot e)
                                    Right (db, hdRoot') -> return $ Right (db, hdRoot')

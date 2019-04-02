{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}

module Test.Integration.Framework.DSL
    (
    -- * Scenario
      scenario
    , xscenario
    , pendingWith
    , Scenarios
    , Context(..)

    -- * Steps
    , Setup(..)
    , setup
    , request
    , request_
    , successfulRequest
    , unsafeRequest
    , verify

    -- * Requests (Only API types)
    , AddressPoolGap
    , AssuranceLevel(..)
    , DestinationChoice(..)
    , EosWallet(..)
    , FilterOperations(..)
    , NewAccount (..)
    , NewAddress(..)
    , NewEosWallet (..)
    , NewWallet (..)
    , PasswordUpdate (..)
    , Payment (..)
    , RawPassword (..)
    , Redemption (..)
    , ShieldedRedemptionCode (..)
    , SortOperations(..)
    , WalletOperation(..)
    , WalletUpdate(..)
    , defaultAccountId
    , defaultAssuranceLevel
    , defaultDistribution
    , defaultAddressPoolGap
    , customDistribution
    , defaultGroupingPolicy
    , defaultPage
    , defaultPerPage
    , defaultSetup
    , defaultSource
    , defaultSpendingPassword
    , defaultWalletName
    , mkSpendingPassword
    , mkPassword
    , noRedemptionMnemonic
    , noSpendingPassword
    , noAddressPoolGap

    -- * Expectations
    , WalletError(..)
    , ErrNotEnoughMoney(..)
    , TransactionStatus(..)
    , expectAddressInIndexOf
    , expectListSizeEqual
    , expectListItemFieldEqual
    , expectEqual
    , expectError
    , expectFieldEqual
    , expectFieldDiffer
    , expectJSONError
    , expectSuccess
    , expectTxInHistoryOf
    , expectTxStatusEventually
    , expectTxStatusNever
    , expectWalletError
    , expectWalletEventuallyRestored
    , expectWalletUTxO

    -- * Helpers
    , ($-)
    , (</>)
    , (!!)
    , address
    , addresses
    , addressPoolGap
    , amount
    , assuranceLevel
    , backupPhrase
    , createdAt
    , externallyOwnedAccounts
    , failures
    , fromWalletId
    , initialCoins
    , mnemonicWords
    , rawAddressPoolGap
    , rawMnemonicPassword
    , rawPassword
    , spendingPassword
    , spendingPasswordLastUpdate
    , totalSuccess
    , walAddresses
    , wallet
    , walletId
    , walletName
    , wallets
    , json
    , hasSpendingPassword
    , mkAddress
    , mkBackupPhrase
    ) where

import           Universum hiding (getArgs, second)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Crypto.Hash (hash)
import           Crypto.Hash.Algorithms (Blake2b_256)
import           Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteArray as ByteArray
import           Data.Default (Default (..))
import qualified Data.Foldable as F
import           Data.Generics.Internal.VL.Lens (lens)
import           Data.Generics.Product.Fields (field)
import           Data.Generics.Product.Typed (HasType, typed)
import           Data.List ((!!))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable (typeOf, typeRepTyCon)
import           Formatting (build, sformat)
import           Language.Haskell.TH.Quote (QuasiQuoter)
import           Test.Hspec.Core.Spec (SpecM, it, xit)
import qualified Test.Hspec.Core.Spec as H
import           Test.Hspec.Expectations.Lifted
import           Test.QuickCheck (arbitrary, generate)
import           Web.HttpApiData (ToHttpApiData (..))

import           Seal.Mnemonic (mkMnemonic, mnemonicToSeed)
import           Seal.Wallet.API.Request.Filter (FilterOperations (..))
import           Seal.Wallet.API.Request.Pagination (Page, PerPage)
import           Seal.Wallet.API.Request.Sort (SortOperations (..))
import           Seal.Wallet.API.Response (JSONValidationError (..))
import           Seal.Wallet.API.V1.Types
import           Seal.Wallet.Client.Http (BaseUrl, ClientError (..), Manager,
                     WalletClient)
import qualified Seal.Wallet.Client.Http as Client
import           Seal.Wallet.Kernel.AddressPoolGap (AddressPoolGap,
                     mkAddressPoolGap)
import           Seal.Wallet.Kernel.Ed25519Bip44 (deriveAccountPrivateKey,
                     derivePublicKey, genEncryptedSecretKey)
import           Seal.Chain.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Seal.Core (Coin, IsBootstrapEraAddr (..), deriveLvl2KeyPair,
                     mkCoin, unsafeGetCoin)
import           Seal.Core.NetworkMagic (NetworkMagic (..))
import           Seal.Crypto (ShouldCheckPassphrase (..),
                     safeDeterministicKeyGen)
import           Test.Integration.Framework.Request (HasHttpClient, request,
                     request_, successfulRequest, unsafeRequest, ($-))
import           Test.Integration.Framework.Scenario (Scenario)
--
-- SCENARIO
--

-- Prior to starting integration tests, we setup a global context
-- and "prepare" a few faucet wallets which all contains some funds.
-- This helps speed up testing and isolate them.
data Context = Context
    { _faucets
        :: [FilePath]
        -- Already funded faucet wallets
    , _client
        :: WalletClient IO
        -- A handle to the underlying wallet backend server
    , _manager
        :: (BaseUrl, Manager)
        -- The underlying BaseUrl and Manager used by the Wallet Client
    } deriving (Generic)


-- | Just a type-alias to 'SpecM', like 'scenario'. Ultimately, everything is
-- made in such way that we can use normal (albeit lifted) HSpec functions and
-- utilities if needed (and rely on its CLI as well when needed).
type Scenarios ctx = SpecM (MVar ctx) ()

-- | Just a slightly-specialized alias for 'it' to help lil'GHC. Also, makes
-- sure the wallet has been cleared out completely before running the scenario.
scenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
scenario title spec = it title (successfulRequest Client.resetWalletState >> spec)

xscenario
    :: String
    -> Scenario Context IO ()
    -> Scenarios Context
xscenario = xit

-- | Lifted version of `H.pendingWith` allowing for temporarily skipping
-- scenarios from execution with a reason, like:
--
--      scenario title $ do
--          pendingWith "This test fails due to bug #213"
--          test
pendingWith
    :: (MonadIO m, MonadFail m)
    => String
    -> m ()
pendingWith = liftIO . H.pendingWith

--
-- TYPES
--

data DestinationChoice
    = RandomDestination
    | LockedDestination
    deriving (Show, Generic)

newtype RawPassword = RawPassword { getRawPassword :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Monoid, Semigroup)

instance IsString RawPassword where
    fromString = RawPassword . T.pack


--
-- STEPS
--

data Setup = Setup
    { _initialCoins
        :: [Coin]
    , _walletName
        :: Text
    , _assuranceLevel
        :: AssuranceLevel
    , _mnemonicWords
        :: [Text]
    , _rawPassword
        :: RawPassword
    , _rawMnemonicPassword
        :: RawPassword
    , _rawAddressPoolGap
        :: Word8
    } deriving (Show, Generic)

data Fixture = Fixture
    { _wallet
        :: Wallet
    , _destinations
        :: NonEmpty Address
    , _backupPhrase
        :: BackupPhrase
    , _spendingPassword
        :: SpendingPassword
    , _eoAccounts
        :: [AccountPublicKeyWithIx]
        -- ^ WARNING We rely on lazyness here, so don't make this strict
    , _addressPoolGap
        :: AddressPoolGap
    } deriving (Show, Generic)

-- | Setup a wallet with the given parameters.
setup
    :: Setup
    -> Scenario Context IO Fixture
setup args = do
    phrase <- if null (args ^. mnemonicWords)
        then liftIO $ generate arbitrary
        else mkBackupPhrase (args ^. mnemonicWords)
    let walPwd@(WalletPassPhrase pwd) = mkPassword (args ^. rawPassword)
    wal <- setupWallet args phrase walPwd
    addrs  <- forM (RandomDestination :| []) setupDestination
    let accs = genExternallyOwnedAccounts (phrase, args ^. rawMnemonicPassword) pwd
    gap <- unsafeMkAddressPoolGap  (args ^. rawAddressPoolGap)
    return $ Fixture wal addrs phrase walPwd accs gap

-- | Apply 'a' to all actions in sequence
verify :: (Monad m) => a -> [a -> m ()] -> m ()
verify a = mapM_ (a &)


--
-- DEFAULT VALUES
--

defaultAccountId :: AccountIndex
defaultAccountId = minBound

defaultAddressPoolGap :: AddressPoolGap
defaultAddressPoolGap = def

defaultAssuranceLevel :: AssuranceLevel
defaultAssuranceLevel = NormalAssurance

defaultDistribution
    :: HasType (NonEmpty Address) s
    => Word64
    -> s
    -> NonEmpty PaymentDistribution
defaultDistribution c s = pure $
    PaymentDistribution (WalAddress $ head $ s ^. typed) (WalletCoin $ mkCoin c)

customDistribution
    :: NonEmpty (Account,Word64)
    -> NonEmpty PaymentDistribution
customDistribution payees =
    let recepientWalAddresses = NonEmpty.fromList
                                $ map (view walAddresses)
                                $ concatMap (view addresses . fst)
                                $ payees
    in NonEmpty.zipWith
       PaymentDistribution
       recepientWalAddresses
       (map ((\coin -> WalletCoin $ mkCoin coin) . snd) payees)


defaultGroupingPolicy :: Maybe WalletInputSelectionPolicy
defaultGroupingPolicy = Nothing

defaultPage :: Maybe Page
defaultPage = Nothing

defaultPerPage :: Maybe PerPage
defaultPerPage = Nothing

defaultSetup :: Setup
defaultSetup = Setup
    { _initialCoins        = []
    , _walletName          = defaultWalletName
    , _assuranceLevel      = defaultAssuranceLevel
    , _mnemonicWords       = []
    , _rawPassword         = mempty
    , _rawMnemonicPassword = mempty
    , _rawAddressPoolGap   = 20
    }

defaultSource
    :: HasType Wallet s
    => s
    -> PaymentSource
defaultSource s =
    PaymentSource (s ^. wallet . walletId) defaultAccountId

defaultSpendingPassword :: SpendingPassword
defaultSpendingPassword = mempty

defaultWalletName :: Text
defaultWalletName = "Fixture Wallet"

noRedemptionMnemonic :: Maybe RedemptionMnemonic
noRedemptionMnemonic = Nothing

noSpendingPassword :: Maybe SpendingPassword
noSpendingPassword = Nothing

noAddressPoolGap :: Maybe AddressPoolGap
noAddressPoolGap = Nothing

--
-- HELPERS
--

json :: QuasiQuoter
json = aesonQQ

infixr 5 </>
(</>) :: ToHttpApiData a => Text -> a -> Text
base </> next = mconcat [base, "/", toQueryParam next]

address :: HasType WalAddress s => Lens' s WalAddress
address = typed

addressPoolGap :: HasType AddressPoolGap s => Lens' s AddressPoolGap
addressPoolGap = typed

amount :: HasType WalletCoin s => Lens' s Word64
amount =
    lens _get _set
  where
    _get :: HasType WalletCoin s => s -> Word64
    _get = unsafeGetCoin . unWalletCoin . view typed
    _set :: HasType WalletCoin s => (s, Word64) -> s
    _set (s, v) = set typed (WalletCoin $ mkCoin v) s

assuranceLevel :: HasType AssuranceLevel s => Lens' s AssuranceLevel
assuranceLevel = typed

backupPhrase :: HasType BackupPhrase s => Lens' s BackupPhrase
backupPhrase = typed

createdAt :: HasType WalletTimestamp s => Lens' s WalletTimestamp
createdAt = typed

externallyOwnedAccounts :: HasType [AccountPublicKeyWithIx] s => Lens' s [AccountPublicKeyWithIx]
externallyOwnedAccounts = typed

failures :: Lens' (BatchImportResult a) [a]
failures = field @"aimFailures"

faucets :: HasType [FilePath] s => Lens' s [FilePath]
faucets = typed

initialCoins
    :: HasType [Coin] s
    => Lens' s [Word64]
initialCoins =
    lens _get _set
  where
    _get :: HasType [Coin] s => s -> [Word64]
    _get = map unsafeGetCoin . view typed
    _set :: HasType [Coin] s => (s, [Word64]) -> s
    _set (s, v) = set typed (map mkCoin v) s

mnemonicWords :: HasType [Text] s => Lens' s [Text]
mnemonicWords = typed

hasSpendingPassword :: HasType Bool s => Lens' s Bool
hasSpendingPassword = typed

rawAddressPoolGap :: Lens' Setup Word8
rawAddressPoolGap = field @"_rawAddressPoolGap"

rawPassword :: Lens' Setup RawPassword
rawPassword = field @"_rawPassword"

rawMnemonicPassword :: Lens' Setup RawPassword
rawMnemonicPassword = field @"_rawMnemonicPassword"

spendingPassword :: HasType SpendingPassword s => Lens' s SpendingPassword
spendingPassword = typed

totalSuccess :: Lens' (BatchImportResult a) Natural
totalSuccess = field @"aimTotalSuccess"

wallet :: HasType Wallet s => Lens' s Wallet
wallet = typed

wallets :: HasType [Wallet] s => Lens' s [Wallet]
wallets = typed

walletId :: HasType WalletId s => Lens' s WalletId
walletId = typed

walletName :: HasType Text s => Lens' s Text
walletName = typed

spendingPasswordLastUpdate :: Lens' Wallet WalletTimestamp
spendingPasswordLastUpdate = field @"walSpendingPasswordLastUpdate"

addresses :: HasType [WalletAddress] s => Lens' s [WalletAddress]
addresses = typed

walAddresses :: HasType WalAddress s => Lens' s WalAddress
walAddresses = typed

--
-- EXPECTATIONS
--


-- | Expects data list returned by the API to be of certain length
expectListSizeEqual
    :: (MonadIO m, MonadFail m, Foldable xs)
    => Int
    -> Either ClientError (xs a)
    -> m ()
expectListSizeEqual l = \case
    Left e   -> wantedSuccessButError e
    Right xs -> length (F.toList xs) `shouldBe` l

-- | Expects that returned data list's particular item field matches the expected value
--
--   e.g.
--     verify response
--          [ expectDataListItemFieldEqual 0 walletName "first"
--          , expectDataListItemFieldEqual 1 walletName "second"
--          ]
expectListItemFieldEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Int
    -> Lens' s a
    -> a
    -> Either ClientError [s]
    -> m ()
expectListItemFieldEqual i getter a = \case
    Left e -> wantedSuccessButError e
    Right s
        | length s > i -> expectFieldEqual getter a (Right (s !! i))
        | otherwise    -> fail $
            "expectListItemFieldEqual: trying to access the #" <> show i <>
            " element from a list but there's none! "

-- | The type signature is more scary than it seems. This drills into a given
--   `a` type through the provided lens and sees whether field matches.
--
--   e.g.
--     do
--       expectFieldEqual #walAssuranceLevel AssuranceStrict response
--       expectFieldEqual #walName "My Wallet" response
expectFieldEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Lens' s a
    -> a
    -> Either ClientError s
    -> m ()
expectFieldEqual getter a = \case
    Left e  -> wantedSuccessButError e
    Right s -> view getter s `shouldBe` a

-- | The opposite to 'expectFieldEqual'.
expectFieldDiffer
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => Lens' s a
    -> a
    -> Either ClientError s
    -> m ()
expectFieldDiffer getter a = \case
    Left e  -> wantedSuccessButError e
    Right s -> view getter s `shouldNotBe` a

-- | Expects entire equality of two types
expectEqual
    :: (MonadIO m, MonadFail m, Show a, Eq a)
    => a
    -> Either ClientError a
    -> m ()
expectEqual =
    expectFieldEqual id

-- | Expect an errored response, without any further assumptions
expectError
    :: (MonadIO m, MonadFail m, Show a)
    => Either ClientError a
    -> m ()
expectError = \case
    Left _  -> return ()
    Right a -> wantedErrorButSuccess a


-- | Expect a successful response, without any further assumptions
expectSuccess
    :: (MonadIO m, MonadFail m, Show a)
    => Either ClientError a
    -> m ()
expectSuccess = \case
    Left e  -> wantedSuccessButError e
    Right _ -> return ()


-- | Expect a transaction to be part of a wallet history.
expectTxInHistoryOf
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => Wallet
    -> Either ClientError Transaction
    -> m ()
expectTxInHistoryOf w = \case
    Left e    -> wantedSuccessButError e
    Right txn -> tryNextPage (on (==) txId txn) 1
  where
    tryNextPage predicate i = do
        txns <- successfulRequest $ Client.getTransactionIndexFilterSorts
            $- Just (walId w)
            $- Nothing
            $- Nothing
            $- Just (fromInteger i)
            $- Just 50
            $- NoFilters
            $- NoSorts
        when (null txns) $
            fail "expectTxInHistoryOf: couldn't find transaction in history"
        case find predicate txns of
            Nothing -> tryNextPage predicate (i + 1)
            Just _  -> return ()


-- | Expect an address to be part of the global index
expectAddressInIndexOf
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    =>  Either ClientError WalletAddress
    -> m ()
expectAddressInIndexOf = \case
    Left e  -> wantedSuccessButError e
    Right addr -> tryNextPage ((==) addr) 1
  where
    tryNextPage predicate i = do
        addrs <- successfulRequest $ Client.getAddressIndexPaginated
            $- Just (fromInteger i)
            $- Just 50
        when (null addrs) $
            fail "expectAddressInIndexOf: couldn't find address in history"
        case find predicate addrs of
            Nothing -> tryNextPage predicate (i + 1)
            Just _  -> return ()


-- | Wait for a transaction to reach one of the given status. Fails after 60
-- seconds if not.
expectTxStatusEventually
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => [TransactionStatus]
    -> Either ClientError Transaction
    -> m ()
expectTxStatusEventually statuses = \case
    Left e    -> wantedSuccessButError e
    Right txn -> do
        result <- ask >>= \ctx -> timeout (120 * second) (waitForTxStatus ctx statuses txn)
        case result of
            Nothing -> fail "expectTxStatusEventually: waited too long for statuses."
            Just _  -> return ()


-- | Checks that a transacton "never" reaches one of the given status. Never
-- really means 60 seconds, you know...
expectTxStatusNever
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => [TransactionStatus]
    -> Either ClientError Transaction
    -> m ()
expectTxStatusNever statuses = \case
    Left e    -> wantedSuccessButError e
    Right txn -> do
        result <- ask >>= \ctx -> timeout (120 * second) (waitForTxStatus ctx statuses txn)
        case result of
            Nothing -> return ()
            Just _  -> fail "expectTxStatusNever: reached one of the provided statuses."


-- | Wait until a wallet is restored, up to a certain point.
expectWalletEventuallyRestored
    :: (MonadIO m, MonadFail m, MonadReader ctx m, HasHttpClient ctx)
    => Either ClientError Wallet
    -> m ()
expectWalletEventuallyRestored = \case
    Left e -> wantedSuccessButError e
    Right w -> do
        result <- ask >>= \ctx -> timeout (120 * second) (waitForRestored ctx w)
        case result of
            Nothing -> fail "expectWalletEventuallyRestored: waited too long for restoration."
            Just _  -> return ()

expectWalletError
    :: (MonadIO m, MonadFail m, Show a)
    => WalletError
    -> Either ClientError a
    -> m ()
expectWalletError e' = \case
    Right a -> wantedErrorButSuccess a
    Left (ClientWalletError e) -> constructorOf e `shouldBe` constructorOf e'
    Left e  -> e `shouldBe` (ClientWalletError e')
  where
    constructorOf = typeRepTyCon . typeOf

-- | Verifies that the response is errored from a failed JSON validation
-- matching part of the given message.
expectJSONError
    :: (MonadIO m, MonadFail m, Show a)
    => String
    -> Either ClientError a
    -> m ()
expectJSONError excerpt = \case
    Right a -> wantedErrorButSuccess a
    Left (ClientJSONError (JSONValidationFailed msg)) ->
        T.unpack msg `shouldContain` excerpt
    Left e ->
        fail $ "expectJSONError: got something else than a JSON validation failure: " <> show e

expectWalletUTxO
    :: (MonadIO m, MonadFail m)
    => [Word64]
    -> Either ClientError UtxoStatistics
    -> m ()
expectWalletUTxO coins = \case
    Left e  -> wantedSuccessButError e
    Right stats -> do
        addr <- liftIO $ generate arbitrary
        let constructUtxoEntry input coin =
                ( TxInUnknown input "arbitrary input"
                , TxOutAux (TxOut addr (mkCoin coin))
                )
        let utxo = Map.fromList $ zipWith constructUtxoEntry [0..] coins

        computeUtxoStatistics log10 [utxo] `shouldBe` stats

--
-- INTERNALS
--

fromWalletId :: Client.WalletId -> Text
fromWalletId (Client.WalletId a) = a

wantedSuccessButError
    :: (MonadFail m, Show e)
    => e
    -> m void
wantedSuccessButError =
    fail . ("expected a successful response but got an error: " <>) . show

wantedErrorButSuccess
    :: (MonadFail m, Show a)
    => a
    -> m void
wantedErrorButSuccess =
    fail . ("expected an error but got a successful response: " <>) . show

timeout :: (MonadIO m) => Int -> IO a -> m (Maybe a)
timeout maxWaitingTime action = liftIO $ do
    race (threadDelay maxWaitingTime) action >>= \case
        Left _  -> return Nothing
        Right a -> return (Just a)

second :: Int
second = 1000000

-- | Wait until the given transaction reaches the given status. Potentially
-- loop ad infinitum; Caller is expected to cancel the thread at some point.
waitForTxStatus
    :: HasHttpClient ctx
    => ctx
    -> [TransactionStatus]
    -> Transaction
    -> IO ()
waitForTxStatus ctx statuses txn = do
    -- NOTE
    -- A bit tricky here, we can't just fire async operation on anything else
    -- but plain `IO`. Hence the explicit context passing here.
    txns <- flip runReaderT ctx $ successfulRequest $ Client.getTransactionIndex
        $- Nothing
        $- Nothing
        $- Nothing

    let tx = find (on (==) txId txn) txns
    if ((fmap txStatus tx) `elem` (fmap Just statuses)) then
        return ()
    else
        threadDelay (5 * second) >> waitForTxStatus ctx statuses txn

-- | Wait until the given wallet is restored.
waitForRestored
    :: HasHttpClient ctx
    => ctx
    -> Wallet
    -> IO ()
waitForRestored ctx w = do
    response <- flip runReaderT ctx $ successfulRequest $ Client.getWallet
        $- w ^. walletId

    case walSyncState response of
        Synced -> return ()
        _      -> threadDelay (5  * second) >> waitForRestored ctx w


-- | Make a backup phrase from a raw list of words.
mkBackupPhrase
    :: (MonadIO m, MonadFail m)
    => [Text]
    -> m BackupPhrase
mkBackupPhrase ws = either onError onSuccess (mkMnemonic ws)
  where
    onError err =
        fail $ "Invalid BackupPhrase provided: " <> show ws <> ". We expect 12\
            \ valid english mnemonic words but the following error has occured:"
            <> show err
    onSuccess =
        return . BackupPhrase

-- | Create a Base16-encoded spending password from raw text
mkPassword
    :: RawPassword
    -> SpendingPassword
mkPassword (RawPassword txt)
    | null txt  = mempty
    | otherwise = txt
        & T.encodeUtf8
        & hash @ByteString @Blake2b_256
        & ByteArray.convert
        & WalletPassPhrase


mkAddress
    :: (MonadIO m, MonadFail m)
    => BackupPhrase
    -> AccountIndex
    -> Word32
    -> m WalAddress
mkAddress (BackupPhrase mnemonic) (accId) ix = do
    let (_, esk) = safeDeterministicKeyGen
            (mnemonicToSeed mnemonic)
            mempty

    let maddr = fst <$> deriveLvl2KeyPair
            NetworkMainOrStage
            (IsBootstrapEraAddr True)
            (ShouldCheckPassphrase False)
            mempty
            esk
            (getAccIndex accId)
            ix

    case maddr of
        Nothing ->
            fail "The impossible happened: failed to generate a\
                \ random address. This can only happened if you\
                \ provided a derivation index that is out-of-bound!"
        Just addr ->
            return (WalAddress addr)


-- | Execute the given setup action with using the next faucet wallet. It fails
-- hard if there's no more faucet wallet available.
withNextFaucet
    :: (Wallet -> Scenario Context IO c)
    -> Scenario Context IO c
withNextFaucet actionWithFaucet = do
    ctx <- get

    when (null $ ctx ^. faucets) $ fail $
        "\nFailed to setup new scenario: there's no more available faucet wallets!\
        \\nWe import a faucet wallet for each scenario but only have a limited\
        \ number of them. This can be modified directly in the configuration file,\
        \ by default in: \n\n\ttest/integration/configuration.yaml\
        \\n\ntry increasing the number of available 'poors' wallets\
        \\n\nspec: &default_core_genesis_spec\
        \\n\tinitializer:\
        \\n\t\ttestBalance:\
        \\n\t\t\tpoors: ???\n"

    let acquireFaucet = do
            let (key:rest) = ctx ^. faucets
            put (ctx & faucets .~ rest)
            successfulRequest $ Client.importWallet $- WalletImport Nothing key

    let releaseFaucet faucet = do
            successfulRequest (Client.deleteWallet $- walId faucet)

    bracket acquireFaucet releaseFaucet actionWithFaucet


-- | Setup a given wallet and pre-fill it with given coins.
setupWallet
    :: Setup
    -> BackupPhrase
    -> SpendingPassword
    -> Scenario Context IO Wallet
setupWallet args phrase password = do
    wal <- successfulRequest $ Client.postWallet $- NewWallet
        phrase
        (Just $ password)
        (args ^. assuranceLevel)
        (args ^. walletName)
        CreateWallet

    unless (null $ args ^. initialCoins) $ withNextFaucet $ \faucet -> do
        let paymentSource = PaymentSource (walId faucet) minBound
        let paymentDist (addr, coin) = pure $ PaymentDistribution (addrId addr) (WalletCoin coin)

        forM_ (args ^. initialCoins) $ \coin -> do
            -- NOTE
            -- Making payments to a different address each time to cope with
            -- grouping policy. That's actually a behavior we might want to
            -- test in the future. So, we'll need to do something smarter here.

            addr <- successfulRequest $ Client.postAddress $- NewAddress
                (Just $ password)
                minBound
                (walId wal)

            txn <- request $ Client.postTransaction $- Payment
                paymentSource
                (paymentDist (addr, mkCoin coin))
                Nothing
                Nothing
            expectTxStatusEventually [InNewestBlocks, Persisted] txn

    return wal


-- | Generate some destinations for payments.
--
--   - RandomDestination generates fake addresses going nowhere (hopefully :) ...)
--   - LockedDestination generates addresses that points to an asset-locked wallet
setupDestination
    :: DestinationChoice
    -> Scenario Context IO Address
setupDestination = \case
    RandomDestination -> do
        bp <- liftIO (generate arbitrary)
        unWalAddress <$> mkAddress bp defaultAccountId 1
    LockedDestination ->
        fail "Asset-locked destination aren't yet implemented. This\
            \ requires slightly more work than it seems and will be\
            \ implemented later."


-- | Lazily generate an "infinite" list of accounts (PubKey, Index)
-- from a mnemonic using the new address derivation scheme V2 (BIP-44 with
-- Ed25519 elliptic curve)
genExternallyOwnedAccounts
    :: (BackupPhrase, RawPassword)
    -> PassPhrase
    -> [AccountPublicKeyWithIx]
genExternallyOwnedAccounts (BackupPhrase mnemonic, RawPassword mnePassphrase) passphrase =
    let
        esk = genEncryptedSecretKey (mnemonic, T.encodeUtf8 mnePassphrase) passphrase
        accountXPrv = fmap derivePublicKey . deriveAccountPrivateKey passphrase esk
    in
        catMaybes
        [ AccountPublicKeyWithIx <$> accountXPrv (getAccIndex ix) <*> pure ix
        | ix <- [minBound..maxBound]
        ]

-- | Create an AddressPoolGap from a raw Word32 and throw in IO if anything
-- goes wrong.
unsafeMkAddressPoolGap
    :: (MonadIO m, MonadFail m)
    => Word8
    -> m AddressPoolGap
unsafeMkAddressPoolGap g = either onError return $ mkAddressPoolGap g
  where
    onError = fail . toString . sformat build

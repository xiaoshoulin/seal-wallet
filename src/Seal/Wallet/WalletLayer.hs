module Seal.Wallet.WalletLayer
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Types
    , CreateWallet(..)
    -- ** Errors
    , CreateWalletError(..)
    , GetWalletError(..)
    , GetAddressPoolGapError(..)
    , GetEosWalletError(..)
    , UpdateWalletError(..)
    , UpdateEosWalletError(..)
    , UpdateWalletPasswordError(..)
    , DeleteWalletError(..)
    , GetUtxosError(..)
    , NewPaymentError(..)
    , EstimateFeesError(..)
    , CreateAddressError(..)
    , ValidateAddressError(..)
    , ImportAddressError(..)
    , CreateAccountError(..)
    , GetAccountError(..)
    , GetAccountsError(..)
    , GetTxError(..)
    , DeleteAccountError(..)
    , UpdateAccountError(..)
    , ImportWalletError(..)
    , NewUnsignedTransactionError(..)
    , SubmitSignedTransactionError(..)
    ) where

import           Universum

import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import qualified Prelude
import           Test.QuickCheck (Arbitrary (..), oneof)

import           Seal.Chain.Block (Blund)
import           Seal.Chain.Genesis
import           Seal.Chain.Txp (Tx, Utxo)
import           Seal.Core (Coin)
import qualified Seal.Core as Core
import           Seal.Core.Chrono (NE, NewestFirst (..), OldestFirst (..))
import           Seal.Core.NetworkMagic (NetworkMagic)
import           Seal.Crypto (EncryptedSecretKey)

import           Seal.Wallet.API.Request (RequestParams (..))
import           Seal.Wallet.API.Request.Filter (FilterOperations (..))
import           Seal.Wallet.API.Request.Sort (SortOperations (..))
import           Seal.Wallet.API.Response (APIResponse, SliceOf (..))
import           Seal.Wallet.API.V1.Types (Account, AccountBalance,
                     AccountIndex, AccountUpdate, BatchImportResult, EosWallet,
                     NewAccount, NewAddress, NewEosWallet, NewWallet,
                     PasswordUpdate, Payment, SignedTransaction,
                     SpendingPassword, Transaction, UnsignedTransaction,
                     UpdateEosWallet, WalAddress, Wallet, WalletAddress,
                     WalletId, WalletImport, WalletTimestamp, WalletTxId,
                     WalletUpdate)
import qualified Seal.Wallet.Kernel.Accounts as Kernel
import qualified Seal.Wallet.Kernel.Addresses as Kernel
import           Seal.Wallet.Kernel.CoinSelection.FromGeneric
                     (ExpenseRegulation, InputGrouping)
import qualified Seal.Wallet.Kernel.DB.HdWallet as Kernel
import           Seal.Wallet.Kernel.DB.TxMeta.Types
import           Seal.Wallet.Kernel.DB.Util.IxSet (IxSet)
import           Seal.Wallet.Kernel.Read (GetAddressPoolGapError (..))
import qualified Seal.Wallet.Kernel.Transactions as Kernel
import qualified Seal.Wallet.Kernel.Wallets as Kernel
import           Seal.Wallet.WalletLayer.ExecutionTimeLimit
                     (TimeExecutionLimit)

------------------------------------------------------------
-- Type & Errors when manipulating wallets
------------------------------------------------------------

data CreateWallet =
    CreateWallet NewWallet
  | ImportWalletFromESK EncryptedSecretKey (Maybe SpendingPassword)

data CreateWalletError =
    CreateWalletError Kernel.CreateWalletError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

instance Arbitrary CreateWalletError where
    arbitrary = oneof [ CreateWalletError <$> arbitrary
                      ]

instance Buildable CreateWalletError where
    build (CreateWalletError kernelError) =
        bprint ("CreateWalletError " % build) kernelError

data GetWalletError =
      GetWalletError Kernel.UnknownHdRoot
    | GetWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | GetWalletWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetWalletError where
    show = formatToString build

instance Exception GetWalletError

instance Buildable GetWalletError where
    build (GetWalletError kernelError) =
        bprint ("GetWalletError " % build) kernelError
    build (GetWalletErrorNotFound walletId) =
        bprint ("GetWalletErrorNotFound " % build) walletId
    build (GetWalletWalletIdDecodingFailed txt) =
        bprint ("GetWalletWalletIdDecodingFailed " % build) txt

data GetEosWalletError =
      GetEosWalletError Kernel.UnknownHdRoot
    | GetEosWalletWalletIdDecodingFailed Text
    | GetEosWalletErrorAddressPoolGap GetAddressPoolGapError
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetEosWalletError where
    show = formatToString build

instance Exception GetEosWalletError

instance Buildable GetEosWalletError where
    build (GetEosWalletError kernelError) =
        bprint ("GetEosWalletError " % build) kernelError
    build (GetEosWalletWalletIdDecodingFailed txt) =
        bprint ("GetEosWalletWalletIdDecodingFailed " % build) txt
    build (GetEosWalletErrorAddressPoolGap gapError) =
        bprint ("GetEosWalletErrorAddressPoolGap " % build) gapError

data UpdateEosWalletError =
      UpdateEosWalletError Kernel.UnknownHdRoot
    | UpdateEosWalletAccountError Kernel.UpdateGapError
    | UpdateEosWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | UpdateEosWalletWalletIdDecodingFailed Text
    | UpdateEosWalletErrorNoAccounts WalletId
    -- ^ Trying to update EOS-wallet which doesn't have any accounts.
    | UpdateEosWalletErrorAddressPoolGap GetAddressPoolGapError
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateEosWalletError where
    show = formatToString build

instance Exception UpdateEosWalletError

instance Buildable UpdateEosWalletError where
    build (UpdateEosWalletError kernelError) =
        bprint ("UpdateEosWalletError " % build) kernelError
    build (UpdateEosWalletAccountError kernelAccError) =
        bprint ("UpdateEosWalletAccountError " % build) kernelAccError
    build (UpdateEosWalletErrorNotFound walletId) =
        bprint ("UpdateEosWalletErrorNotFound " % build) walletId
    build (UpdateEosWalletWalletIdDecodingFailed txt) =
        bprint ("UpdateEosWalletWalletIdDecodingFailed " % build) txt
    build (UpdateEosWalletErrorNoAccounts walletId) =
        bprint ("UpdateEosWalletErrorNoAccounts " % build) walletId
    build (UpdateEosWalletErrorAddressPoolGap gapError) =
        bprint ("UpdateEosWalletErrorAddressPoolGap " % build) gapError

data UpdateWalletError =
      UpdateWalletError Kernel.UnknownHdRoot
    | UpdateWalletErrorNotFound WalletId
    -- ^ Error thrown by the legacy wallet layer, isomorphic to the one above,
    -- which is new-data-layer specific.
    | UpdateWalletWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateWalletError where
    show = formatToString build

instance Exception UpdateWalletError

instance Buildable UpdateWalletError where
    build (UpdateWalletError kernelError) =
        bprint ("UpdateWalletError " % build) kernelError
    build (UpdateWalletErrorNotFound walletId) =
        bprint ("UpdateWalletErrorNotFound " % build) walletId
    build (UpdateWalletWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletWalletIdDecodingFailed " % build) txt

data UpdateWalletPasswordError =
      UpdateWalletPasswordWalletIdDecodingFailed Text
    | UpdateWalletPasswordError Kernel.UpdateWalletPasswordError

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateWalletPasswordError where
    show = formatToString build

instance Exception UpdateWalletPasswordError

instance Buildable UpdateWalletPasswordError where
    build (UpdateWalletPasswordWalletIdDecodingFailed txt) =
        bprint ("UpdateWalletPasswordWalletIdDecodingFailed " % build) txt
    build (UpdateWalletPasswordError kernelError) =
        bprint ("UpdateWalletPasswordError " % build) kernelError

data DeleteWalletError =
      DeleteWalletWalletIdDecodingFailed Text
    | DeleteWalletError Kernel.UnknownHdRoot

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteWalletError where
    show = formatToString build

instance Exception DeleteWalletError

instance Buildable DeleteWalletError where
    build (DeleteWalletWalletIdDecodingFailed txt) =
        bprint ("DeleteWalletWalletIdDecodingFailed " % build) txt
    build (DeleteWalletError kernelError) =
        bprint ("DeleteWalletError " % build) kernelError

data GetUtxosError =
      GetUtxosWalletIdDecodingFailed Text
    | GetUtxosGetAccountsError Kernel.UnknownHdRoot
    | GetUtxosCurrentAvailableUtxoError Kernel.UnknownHdAccount
    deriving Eq

instance Show GetUtxosError where
    show = formatToString build

instance Exception GetUtxosError

instance Buildable GetUtxosError where
    build (GetUtxosWalletIdDecodingFailed txt) =
        bprint ("GetUtxosWalletIdDecodingFailed " % build) txt
    build (GetUtxosGetAccountsError kernelError) =
        bprint ("GetUtxosGetAccountsError " % build) kernelError
    build (GetUtxosCurrentAvailableUtxoError kernelError) =
        bprint ("GetUtxosCurrentAvailableUtxoError " % build) kernelError

------------------------------------------------------------
-- Errors when dealing with addresses
------------------------------------------------------------

data CreateAddressError =
      CreateAddressError Kernel.CreateAddressError
    | CreateAddressAddressDecodingFailed Text
    -- ^ Decoding the input 'Text' as an 'Address' failed.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAddressError where
    show = formatToString build

instance Exception CreateAddressError

instance Arbitrary CreateAddressError where
    arbitrary = oneof [ CreateAddressError <$> arbitrary
                      , pure (CreateAddressAddressDecodingFailed "Ae2tdPwUPEZ18ZjTLnLVr9CEvUEUX4eW1LBHbxxx")
                      ]

instance Buildable CreateAddressError where
    build (CreateAddressError kernelError) =
        bprint ("CreateAddressError " % build) kernelError
    build (CreateAddressAddressDecodingFailed txt) =
        bprint ("CreateAddressAddressDecodingFailed " % build) txt

data ValidateAddressError =
      ValidateAddressDecodingFailed Text
    -- ^ When trying to decode this raw 'Text' into a proper Seal
    -- 'Address' the decoding failed. Unfortunately we are not able to
    -- provide a more accurate error description as 'decodeTextAddress' doesn't
    -- offer such.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ValidateAddressError where
    show = formatToString build

instance Exception ValidateAddressError

instance Buildable ValidateAddressError where
    build (ValidateAddressDecodingFailed rawText) =
        bprint ("ValidateAddressDecodingFailed " % build) rawText

data ImportAddressError =
      ImportAddressError Kernel.ImportAddressError
    | ImportAddressAddressDecodingFailed Text
    -- ^ Decoding the input 'Text' as an 'Address' failed.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ImportAddressError where
    show = formatToString build

instance Exception ImportAddressError

instance Arbitrary ImportAddressError where
    arbitrary = oneof [ ImportAddressError <$> arbitrary
                      , pure (ImportAddressAddressDecodingFailed "Ae2tdPwUPEZ18ZjTLnLVr9CEvUEUX4eW1LBHbxxx")
                      ]

instance Buildable ImportAddressError where
    build (ImportAddressError kernelError) =
        bprint ("ImportAddressError " % build) kernelError
    build (ImportAddressAddressDecodingFailed txt) =
        bprint ("ImportAddressAddressDecodingFailed " % build) txt


------------------------------------------------------------
-- Errors when dealing with Accounts
------------------------------------------------------------

data CreateAccountError =
      CreateAccountError Kernel.CreateAccountError
    | CreateAccountWalletIdDecodingFailed Text
    -- ^ Decoding the parent's 'WalletId' from a raw 'Text' failed.
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError

instance Arbitrary CreateAccountError where
    arbitrary = oneof [ CreateAccountError <$> arbitrary
                      , CreateAccountWalletIdDecodingFailed <$> arbitrary
                      ]

instance Buildable CreateAccountError where
    build (CreateAccountError kernelError) =
        bprint ("CreateAccountError " % build) kernelError
    build (CreateAccountWalletIdDecodingFailed txt) =
        bprint ("CreateAccountWalletIdDecodingFailed " % build) txt

data GetAccountError =
      GetAccountError Kernel.UnknownHdAccount
    | GetAccountWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetAccountError where
    show = formatToString build

instance Exception GetAccountError

instance Buildable GetAccountError where
    build (GetAccountError kernelError) =
        bprint ("GetAccountError " % build) kernelError
    build (GetAccountWalletIdDecodingFailed txt) =
        bprint ("GetAccountWalletIdDecodingFailed " % build) txt

data DeleteAccountError =
      DeleteAccountError Kernel.UnknownHdAccount
    | DeleteAccountWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show DeleteAccountError where
    show = formatToString build

instance Exception DeleteAccountError

instance Buildable DeleteAccountError where
    build (DeleteAccountError kernelError) =
        bprint ("DeleteAccountError " % build) kernelError
    build (DeleteAccountWalletIdDecodingFailed txt) =
        bprint ("DeleteAccountWalletIdDecodingFailed " % build) txt

data GetAccountsError =
      GetAccountsError Kernel.UnknownHdRoot
    | GetAccountsWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show GetAccountsError where
    show = formatToString build

instance Exception GetAccountsError

instance Buildable GetAccountsError where
    build (GetAccountsError kernelError) =
        bprint ("GetAccountsError " % build) kernelError
    build (GetAccountsWalletIdDecodingFailed txt) =
        bprint ("GetAccountsWalletIdDecodingFailed " % build) txt

data UpdateAccountError =
      UpdateAccountError Kernel.UnknownHdAccount
    | UpdateAccountWalletIdDecodingFailed Text
    deriving Eq

-- | Unsound show instance needed for the 'Exception' instance.
instance Show UpdateAccountError where
    show = formatToString build

instance Exception UpdateAccountError

instance Buildable UpdateAccountError where
    build (UpdateAccountError kernelError) =
        bprint ("UpdateAccountError " % build) kernelError
    build (UpdateAccountWalletIdDecodingFailed txt) =
        bprint ("UpdateAccountWalletIdDecodingFailed " % build) txt

data ImportWalletError =
      ImportWalletFileNotFound FilePath
    | ImportWalletNoWalletFoundInBackup FilePath
    -- ^ When trying to fetch the required information, the legacy keystore
    -- didn't provide any.
    | ImportWalletCreationFailed CreateWalletError
    -- ^ When trying to import this wallet, the wallet creation failed.

-- | Unsound show instance needed for the 'Exception' instance.
instance Show ImportWalletError where
    show = formatToString build

instance Exception ImportWalletError

instance Buildable ImportWalletError where
    build (ImportWalletFileNotFound fp) =
        bprint ("ImportWalletFileNotFound " % build) fp
    build (ImportWalletNoWalletFoundInBackup fp) =
        bprint ("ImportWalletNoWalletFoundInBackup " % build) fp
    build (ImportWalletCreationFailed err) =
        bprint ("ImportWalletCreationFailed " % build) err

------------------------------------------------------------
-- Errors when getting Transactions
------------------------------------------------------------

data GetTxError =
      GetTxMissingWalletIdError
    | GetTxAddressDecodingFailed Text
    | GetTxInvalidSortingOperation String
    | GetTxUnknownHdAccount Kernel.UnknownHdAccount

instance Show GetTxError where
    show = formatToString build

instance Buildable GetTxError where
    build GetTxMissingWalletIdError =
        bprint "GetTxMissingWalletIdError "
    build (GetTxAddressDecodingFailed txt) =
        bprint ("GetTxAddressDecodingFailed " % build) txt
    build (GetTxInvalidSortingOperation txt) =
        bprint ("GetTxInvalidSortingOperation " % build) txt
    build (GetTxUnknownHdAccount err) =
        bprint ("GetTxUnknownHdAccount " % build) err


instance Arbitrary GetTxError where
    arbitrary = oneof [ pure GetTxMissingWalletIdError
                      , pure (GetTxAddressDecodingFailed "by_amount")
                      , pure (GetTxInvalidSortingOperation "123")
                      , GetTxUnknownHdAccount <$> arbitrary
                      ]

instance Exception GetTxError

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- fully-owned wallets
      createWallet         :: CreateWallet -> m (Either CreateWalletError Wallet)
    , getWallets           :: m (IxSet Wallet)
    , getEosWallets        :: m (Either GetEosWalletError (IxSet EosWallet))
    , getWallet            :: WalletId -> m (Either GetWalletError Wallet)
    , getEosWallet         :: WalletId -> m (Either GetEosWalletError EosWallet)
    , updateWallet         :: WalletId
                           -> WalletUpdate
                           -> m (Either UpdateWalletError Wallet)
    , updateEosWallet      :: WalletId
                           -> UpdateEosWallet
                           -> m (Either UpdateEosWalletError EosWallet)
    , updateWalletPassword :: WalletId
                           -> PasswordUpdate
                           -> m (Either UpdateWalletPasswordError Wallet)
    , deleteWallet         :: WalletId -> m (Either DeleteWalletError ())
    -- externally-owned wallets
    , createEosWallet      :: NewEosWallet -> m (Either CreateWalletError EosWallet)
    , deleteEosWallet      :: WalletId -> m (Either DeleteWalletError ())
    , getUtxos             :: WalletId
                           -> m (Either GetUtxosError [(Account, Utxo)])
    -- accounts
    , createAccount        :: WalletId
                           -> NewAccount
                           -> m (Either CreateAccountError Account)
    , getAccounts          :: WalletId
                           -> m (Either GetAccountsError (IxSet Account))
    , getAccount           :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError Account)
    , getAccountBalance    :: WalletId
                           -> AccountIndex
                           -> m (Either GetAccountError AccountBalance)
    , getAccountAddresses  :: WalletId
                           -> AccountIndex
                           -> RequestParams
                           -> FilterOperations '[WalAddress] WalletAddress
                           -> m (Either GetAccountError (APIResponse [WalletAddress]))
    , updateAccount        :: WalletId
                           -> AccountIndex
                           -> AccountUpdate
                           -> m (Either UpdateAccountError Account)
    , deleteAccount        :: WalletId
                           -> AccountIndex
                           -> m (Either DeleteAccountError ())
    -- addresses
    , createAddress        :: NewAddress
                           -> m (Either CreateAddressError WalletAddress)
    , getAddresses         :: RequestParams -> m (SliceOf WalletAddress)
    , validateAddress      :: Text
                           -> m (Either ValidateAddressError WalletAddress)
    , importAddresses      :: WalletId
                           -> [WalAddress]
                           -> m (Either ImportAddressError (BatchImportResult WalAddress))

    -- transactions
    , getTransactions      :: Maybe WalletId
                           -> Maybe AccountIndex
                           -> Maybe (WalAddress)
                           -> RequestParams
                           -> FilterOperations '[WalletTxId, WalletTimestamp] Transaction
                           -> SortOperations Transaction
                           -> m (Either GetTxError (APIResponse [Transaction]))
    , getTxFromMeta        :: TxMeta -> m (Either Kernel.UnknownHdAccount Transaction)

    -- core API
    , applyBlocks          :: OldestFirst NE Blund -> m ()
    , rollbackBlocks       :: NewestFirst NE Blund -> m ()

    -- internal
    , resetWalletState     :: m ()
    , importWallet         :: WalletImport -> m (Either ImportWalletError Wallet)
    }

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | Performs a payment.
    , pay :: Config
          -> InputGrouping
          -- An preference on how to group inputs during coin selection.
          -> ExpenseRegulation
          -- Who pays the fee, if the sender or the receivers.
          -> Payment
          -- The payment we need to perform.
          -> m (Either NewPaymentError (Tx, TxMeta))

      -- | Estimates the fees for a payment.
    , estimateFees :: InputGrouping
                   -- An preference on how to group inputs during coin selection
                   -> ExpenseRegulation
                   -- Who pays the fee, if the sender or the receivers.
                   -> Payment
                   -- The payment we need to perform.
                   -> m (Either EstimateFeesError Coin)

      -- | Prepares unsigned transaction. Please note that this function does /not/
      -- perform a payment, it just creates a new transaction which will be signed
      -- and submitted to the blockchain later.
      --
      -- It returns transaction and list of the source addresses with corresponding
      -- derivation paths. These addresses and paths will be used by third party to
      -- provide a proof that it has a right to spend money from these addresses.
    , createUnsignedTx :: InputGrouping
                       -- An preference on how to group inputs during coin selection
                       -> ExpenseRegulation
                       -- Who pays the fee, if the sender or the receivers.
                       -> Payment
                       -- The payment we need to perform.
                       -> m (Either NewUnsignedTransactionError UnsignedTransaction)

      -- | Takes externally-signed transaction and submits it to the blockchain.
      -- The result of 'submitSignedTx' is equal to 'pay'.
    , submitSignedTx :: SignedTransaction
                     -> m (Either SubmitSignedTransactionError (Tx, TxMeta))
    }

------------------------------------------------------------
-- Active wallet errors
------------------------------------------------------------

data NewPaymentError =
      NewPaymentError Kernel.PaymentError
    | NewPaymentTimeLimitReached TimeExecutionLimit
    | NewPaymentWalletIdDecodingFailed Text
    | NewPaymentUnknownAccountId Kernel.UnknownHdAccount
    | NewPaymentAddressBadNetworkMagic NetworkMagic (NonEmpty Core.Address)

-- | Unsound show instance needed for the 'Exception' instance.
instance Show NewPaymentError where
    show = formatToString build

instance Exception NewPaymentError

instance Buildable NewPaymentError where
    build (NewPaymentError kernelErr) =
        bprint ("NewPaymentError " % build) kernelErr
    build (NewPaymentTimeLimitReached ter) =
        bprint ("NewPaymentTimeLimitReached " % build) ter
    build (NewPaymentWalletIdDecodingFailed txt) =
        bprint ("NewPaymentWalletIdDecodingFailed " % build) txt
    build (NewPaymentUnknownAccountId err) =
        bprint ("NewPaymentUnknownAccountId " % build) err
    build (NewPaymentAddressBadNetworkMagic expectedNM dstAddrs) =
        bprint ("NewPaymentAddressBadNetworkMagic " % build % " " % build)
               expectedNM
               (toList dstAddrs)


data EstimateFeesError =
      EstimateFeesError Kernel.EstimateFeesError
    | EstimateFeesTimeLimitReached TimeExecutionLimit
    | EstimateFeesWalletIdDecodingFailed Text

-- | Unsound show instance needed for the 'Exception' instance.
instance Show EstimateFeesError where
    show = formatToString build

instance Exception EstimateFeesError

instance Buildable EstimateFeesError where
    build (EstimateFeesError kernelErr) =
        bprint ("EstimateFeesError " % build) kernelErr
    build (EstimateFeesTimeLimitReached ter) =
        bprint ("EstimateFeesTimeLimitReached " % build) ter
    build (EstimateFeesWalletIdDecodingFailed txt) =
        bprint ("EstimateFeesWalletIdDecodingFailed " % build) txt

instance Arbitrary EstimateFeesError where
    arbitrary = oneof [ EstimateFeesError <$> arbitrary
                      , EstimateFeesTimeLimitReached <$> arbitrary
                      ]

data NewUnsignedTransactionError =
      NewUnsignedTransactionError Kernel.NewTransactionError
    | NewTransactionWalletIdDecodingFailed Text

instance Show NewUnsignedTransactionError where
    show = formatToString build

instance Exception NewUnsignedTransactionError

instance Buildable NewUnsignedTransactionError where
    build (NewUnsignedTransactionError err) =
        bprint ("NewUnsignedTransactionError " % build) err
    build (NewTransactionWalletIdDecodingFailed txt) =
        bprint ("NewTransactionWalletIdDecodingFailed " % build) txt

data SubmitSignedTransactionError =
      SubmitSignedTransactionError Kernel.PaymentError
    | SubmitSignedTransactionWalletIdDecodingFailed Text
    | SubmitSignedTransactionNotBase16Format
    | SubmitSignedTransactionUnableToDecode
    | SubmitSignedTransactionInvalidSrcAddress
    | SubmitSignedTransactionSigNotBase16Format
    | SubmitSignedTransactionInvalidSig
    | SubmitSignedTransactionInvalidPK

instance Show SubmitSignedTransactionError where
    show = formatToString build

instance Exception SubmitSignedTransactionError

instance Buildable SubmitSignedTransactionError where
    build (SubmitSignedTransactionError err) =
        bprint ("NewUnsignedTransactionError " % build) err
    build (SubmitSignedTransactionWalletIdDecodingFailed txt) =
        bprint ("NewTransactionWalletIdDecodingFailed " % build) txt
    build SubmitSignedTransactionNotBase16Format =
        bprint ("SubmitSignedTransactionNotBase16Format")
    build SubmitSignedTransactionUnableToDecode =
        bprint ("SubmitSignedTransactionUnableToDecode")
    build SubmitSignedTransactionInvalidSrcAddress =
        bprint ("SubmitSignedTransactionInvalidSrcAddress")
    build SubmitSignedTransactionSigNotBase16Format =
        bprint ("SubmitSignedTransactionSigNotBase16Format")
    build SubmitSignedTransactionInvalidSig =
        bprint ("SubmitSignedTransactionInvalidSig")
    build SubmitSignedTransactionInvalidPK =
        bprint ("SubmitSignedTransactionInvalidPK")

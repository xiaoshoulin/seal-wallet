{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE StrictData    #-}

-- | This module defines a client interface for the Seal wallet.
module Seal.Wallet.Client
    ( -- * The abstract client
      WalletClient(..)
    , WalletDocClient(..)
    , getWalletIndex
    , getAccounts
    , getWallets
    , getAddressIndex
    , getTransactionIndex
    , Resp
    , hoistClient
    , liftClient
    , onClientError
    , withThrottlingRetry
    -- * The type of errors that the client might return
    , ClientError(..)
    , WalletError(..)
    , ServantError(..)
    , Response
    , GenResponse(..)
    -- * Reexports
    , module Seal.Wallet.API.V1.Types
    , module Seal.Wallet.API.V1.Parameters
    , module Seal.Wallet.API.Request.Pagination
    , FilterOperations(..)
    , SortOperations(..)
    , FilterOperation(..)
    , SortOperation(..)
    , APIResponse(..)
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Exception (Exception (..))
import           Data.Swagger (Swagger)
import           Servant.Client (GenResponse (..), Response, ServantError (..))

import qualified Seal.Core as Core

import           Seal.Wallet.API.Request.Filter
import           Seal.Wallet.API.Request.Pagination
import           Seal.Wallet.API.Request.Sort
import           Seal.Wallet.API.Response
import           Seal.Wallet.API.V1.Parameters
import           Seal.Wallet.API.V1.Types

-- | A representation of a wallet client parameterized over some effect
-- type @m@.
--
-- The record fields serve as the API to the wallet client. Note that the
-- field 'getAddressIndex' has the type:
--
-- @
-- 'getAddressIndex'
--     :: 'WalletClient' m
--     -> m ('Either' 'WalletError' ('APIResponse' ['Address']))
-- @
--
-- Other functions may be defined in terms of this 'WalletClient' -- see
-- 'getWalletIndex' as a convenience helper for 'getWalletIndexPaged'.
-- TODO(ks): I don't think that it's important to preserve paging as
-- an important detail, we should remove paging and return the full set
-- of results.
data WalletClient m
    = WalletClient
    { -- address endpoints
      getAddressIndexPaginated
         :: Maybe Page -> Maybe PerPage -> Resp m [WalletAddress]
    , postAddress
         :: NewAddress -> Resp m WalletAddress
    , getAddress
         :: Text -> Resp m WalletAddress
    , importAddresses
        :: WalletId
        -> [WalAddress]
        -> Resp m (BatchImportResult WalAddress)
    -- wallets endpoints
    , postWallet
         :: New Wallet -> Resp m Wallet
    , getWalletIndexFilterSorts
         :: Maybe Page
         -> Maybe PerPage
         -> FilterOperations '[WalletId, Core.Coin] Wallet
         -> SortOperations Wallet
         -> Resp m [Wallet]
    , updateWalletPassword
         :: WalletId -> PasswordUpdate -> Resp m Wallet
    , deleteWallet
         :: WalletId -> m (Either ClientError ())
    , getWallet
        :: WalletId -> Resp m Wallet
    , updateWallet
         :: WalletId -> Update Wallet -> Resp m Wallet
    , getUtxoStatistics
        :: WalletId -> Resp m UtxoStatistics
    , postUnsignedTransaction
         :: Payment -> Resp m UnsignedTransaction
    , postSignedTransaction
         :: SignedTransaction -> Resp m Transaction
    -- account endpoints
    , deleteAccount
         :: WalletId -> AccountIndex -> m (Either ClientError ())
    , getAccount
         :: WalletId -> AccountIndex -> Resp m Account
    , getAccountIndexPaged
         :: WalletId -> Maybe Page -> Maybe PerPage -> Resp m [Account]
    , postAccount
        :: WalletId -> New Account -> Resp m Account
    , updateAccount
         :: WalletId -> AccountIndex -> Update Account -> Resp m Account
    , getAccountAddresses
         :: WalletId
         -> AccountIndex
         -> Maybe Page
         -> Maybe PerPage
         -> FilterOperations '[WalAddress] WalletAddress
         -> Resp m AccountAddresses
    , getAccountBalance
         :: WalletId -> AccountIndex -> Resp m AccountBalance
    -- transactions endpoints
    , postTransaction
         :: Payment -> Resp m Transaction
    , getTransactionIndexFilterSorts
         :: Maybe WalletId
         -> Maybe AccountIndex
         -> Maybe WalAddress
         -> Maybe Page
         -> Maybe PerPage
         -> FilterOperations '[WalletTxId, WalletTimestamp] Transaction
         -> SortOperations Transaction
         -> Resp m [Transaction]
    , getTransactionFee
         :: Payment -> Resp m EstimatedFees
    -- , redeemAda
    --      :: Redemption -> Resp m Transaction
    -- settings
    -- , getNodeSettings
    --      :: Resp m NodeSettings
    -- info
    -- , getNodeInfo
        --  :: ForceNtpCheck -> Resp m NodeInfo

    -- Internal API
    , nextUpdate
        :: Resp m WalletSoftwareVersion
    , applyUpdate
        :: m (Either ClientError ())
    , postponeUpdate
        :: m (Either ClientError ())
    , resetWalletState
        :: m (Either ClientError ())
    , importWallet
        :: WalletImport -> Resp m Wallet

    -- Externally Owned Wallets Endpoints
    , postEosWallet
        :: NewEosWallet
        -> Resp m EosWallet
    , getEosWallet
        :: WalletId
        -> Resp m EosWallet
    , updateEosWallet
        :: WalletId
        -> UpdateEosWallet
        -> Resp m EosWallet
    , deleteEosWallet
        :: WalletId
        -> m (Either ClientError ())
    , getEosWalletIndexFilterSorts
         :: Maybe Page
         -> Maybe PerPage
         -> FilterOperations '[WalletId, Core.Coin] EosWallet
         -> SortOperations EosWallet
         -> Resp m [EosWallet]
    } deriving Generic


data WalletDocClient m = WalletDocClient
    { getSwaggerJson
        :: m Swagger
    } deriving Generic

-- | Paginates through all request pages and concatenates the result.
--
-- NOTE: this lazy variant might be inefficient. It is supposed to be used only in tests. Implement strict version if optimization is needed
-- TODO(akegalj): this can be paralelized like so (pseudo):
--   do
--     -- first page is fetched in sequence
--     page1 <- request (page 1) (Just maxPerPageEntries)
--     -- then rest of the pages is fetched in parallel
--     fromPage2 <- paralelMap (\p -> request p $ Just 50) [2..page1.wrMeta.metaTotalPages]
--     concatMap wrData $ page1:fromPage2
--
paginateAll :: Monad m => (Maybe Page -> Maybe PerPage -> Resp m [a]) -> Resp m [a]
paginateAll request = fmap fixMetadata <$> paginatePage 1
  where
    fixMetadata APIResponse{..} =
        APIResponse
            { wrMeta = Metadata
                PaginationMetadata
                    { metaTotalPages = 1
                    , metaPage = Page 1
                    , metaPerPage = PerPage $ length wrData
                    , metaTotalEntries = length wrData
                    }
            , ..
            }
    paginatePage page = do
        result <- request (Just $ Page page) (Just $ PerPage maxPerPageEntries)
        case result of
            Left _ -> pure result
            Right resp ->
                if null $ wrData resp
                    then pure result
                    else fmap (\d -> d { wrData = wrData resp <> wrData d }) <$> paginatePage (succ page)

getAddressIndex :: Monad m => WalletClient m -> Resp m [WalletAddress]
getAddressIndex = paginateAll . getAddressIndexPaginated

getAccounts :: Monad m => WalletClient m -> WalletId -> Resp m [Account]
getAccounts wc = paginateAll . getAccountIndexPaged wc

getTransactionIndex
    :: Monad m
    => WalletClient m
    -> Maybe WalletId
    -> Maybe AccountIndex
    -> Maybe WalAddress
    -> Resp m [Transaction]
getTransactionIndex wc wid maid maddr =
    paginateAll $ \mp mpp -> getTransactionIndexFilterSorts wc wid maid maddr mp mpp NoFilters NoSorts

getWalletIndexPaged :: WalletClient m -> Maybe Page -> Maybe PerPage -> Resp m [Wallet]
getWalletIndexPaged wc mp mpp = getWalletIndexFilterSorts wc mp mpp NoFilters NoSorts

-- | Retrieves only the first page of wallets, providing a default value to
-- 'Page' and 'PerPage'.
getWallets :: Monad m => WalletClient m -> Resp m [Wallet]
getWallets = paginateAll . getWalletIndexPaged

-- | Natural transformation + mapping on a 'WalletClient'
natMapClient
    :: (forall x. m x -> n x)
    -> (forall x. n (Either ClientError x) -> n (Either ClientError x))
    -> WalletClient m
    -> WalletClient n
natMapClient phi f wc = WalletClient
    { getAddressIndexPaginated =
        \x -> f . phi . getAddressIndexPaginated wc x
    , postAddress =
        f . phi . postAddress wc
    , getAddress =
        f . phi . getAddress wc
    , importAddresses =
        \x -> f . phi . importAddresses wc x
    , postWallet =
        f . phi . postWallet wc
    , getWalletIndexFilterSorts =
        \x y p -> f . phi . getWalletIndexFilterSorts wc x y p
    , updateWalletPassword =
        \x -> f . phi . updateWalletPassword wc x
    , deleteWallet =
        f . phi . deleteWallet wc
    , getWallet =
        f . phi . getWallet wc
    , updateWallet =
        \x -> f . phi . updateWallet wc x
    , getUtxoStatistics =
        f . phi . getUtxoStatistics wc
    , postUnsignedTransaction =
        f . phi . postUnsignedTransaction wc
    , postSignedTransaction =
        f . phi . postSignedTransaction wc
    , deleteAccount =
        \x -> f . phi . deleteAccount wc x
    , getAccount =
        \x -> f . phi . getAccount wc x
    , getAccountIndexPaged =
        \x mp -> f . phi . getAccountIndexPaged wc x mp
    , postAccount =
        \x -> f . phi . postAccount wc x
    , updateAccount =
        \x y -> f . phi . updateAccount wc x y
    -- , redeemAda =
    --     f . phi . redeemAda wc
    , getAccountAddresses =
        \x y p pp ff -> f $ phi $ getAccountAddresses wc x y p pp ff
    , getAccountBalance =
        \x -> f . phi . getAccountBalance wc x
    , postTransaction =
        f . phi . postTransaction wc
    , getTransactionIndexFilterSorts =
        \wid maid maddr mp mpp ff ->
            f . phi . getTransactionIndexFilterSorts wc wid maid maddr mp mpp ff
    , getTransactionFee =
        f . phi . getTransactionFee wc
    -- , getNodeSettings =
    --     f $ phi $ getNodeSettings wc
    -- , getNodeInfo =
    --     f . phi . getNodeInfo wc
    , nextUpdate =
        f $ phi $ nextUpdate wc
    , applyUpdate =
        f $ phi $ applyUpdate wc
    , postponeUpdate =
        f $ phi $ postponeUpdate wc
    , resetWalletState =
        f $ phi $ resetWalletState wc
    , importWallet =
        f . phi . importWallet wc
    , postEosWallet =
        f . phi . postEosWallet wc
    , getEosWallet =
        f . phi . getEosWallet wc
    , updateEosWallet =
        \x -> f . phi . updateEosWallet wc x
    , deleteEosWallet =
        f . phi . deleteEosWallet wc
    , getEosWalletIndexFilterSorts =
        \x y p -> f . phi . getEosWalletIndexFilterSorts wc x y p
    }

-- | Run the given natural transformation over the 'WalletClient'.
hoistClient :: (forall x. m x -> n x) -> WalletClient m -> WalletClient n
hoistClient phi =
    natMapClient phi id

-- | Generalize a @'WalletClient' 'IO'@ into a @('MonadIO' m) =>
-- 'WalletClient' m@.
liftClient :: MonadIO m => WalletClient IO -> WalletClient m
liftClient = hoistClient liftIO


onClientError
    :: forall m. Monad m
    => ResponseErrorHandler m
    -> WalletClient m
    -> WalletClient m
onClientError handler =
    natMapClient id overError
  where
    overError :: m (Either ClientError a) -> m (Either ClientError a)
    overError action = do
        result <- action
        case result of
            Left clientError ->
                handler clientError action
            Right res ->
                pure (Right res)

-- | This function catches the wallet error corresponding to throttling and
-- causes the client to wait for the specified amount of time before retrying.
withThrottlingRetry :: forall m. MonadIO m => WalletClient m -> WalletClient m
withThrottlingRetry = onClientError retry
  where
    fudgeFactor :: Word64 -> Int
    fudgeFactor x = fromIntegral (x + ((x `div` 100) * 5)) -- add 5% to the time

    retry :: ResponseErrorHandler m
    retry err action =
        case err of
            ClientWalletError (RequestThrottled microsTilRetry) -> do
                liftIO (threadDelay (fudgeFactor microsTilRetry))
                newResult <- action
                case newResult of
                    Left err' ->
                        retry err' action
                    Right a ->
                        pure (Right a)
            _ ->
                pure (Left err)

-- | This type represents callbacks you can use to modify how errors are
-- received and reported.
type ResponseErrorHandler m
    = forall x
    . ClientError
    -- ^ The error response received by the client.
    -> m (Either ClientError x)
    -- ^ The action that was performed, if you want to retry the request.
    -> m (Either ClientError x)
    -- ^ The action to


-- | Calls 'getWalletIndexPaged' using the 'Default' values for 'Page' and
-- 'PerPage'.
getWalletIndex :: Monad m => WalletClient m -> Resp m [Wallet]
getWalletIndex = paginateAll . getWalletIndexPaged


-- | A type alias shorthand for the response from the 'WalletClient'.
type Resp m a = m (Either ClientError (APIResponse a))

-- | The type of errors that the wallet might return.
data ClientError
    = ClientWalletError WalletError
    -- ^ The 'WalletError' type represents known failures that the API
    -- might return.
    | ClientJSONError JSONValidationError
    -- ^ Error returned when submitting an invalid JSON object
    | ClientHttpError ServantError
    -- ^ We directly expose the 'ServantError' type as part of this
    | UnknownClientError SomeException
    -- ^ This constructor is used when the API client reports an error that
    -- isn't represented in either the 'ServantError' HTTP errors or the
    -- 'WalletError' for API errors.
    deriving (Show, Generic)

-- | General (and naive) equality instance.
instance Eq ClientError where
    ClientWalletError  e1 == ClientWalletError  e2 = e1 == e2
    ClientJSONError    e1 == ClientJSONError    e2 = e1 == e2
    ClientHttpError    e1 == ClientHttpError    e2 = e1 == e2
    UnknownClientError _  == UnknownClientError _  = True
    _ == _ = False

-- | General exception instance.
instance Exception ClientError where
    toException (ClientWalletError  e) = toException e
    toException (ClientJSONError    e) = toException e
    toException (ClientHttpError    e) = toException e
    toException (UnknownClientError e) = toException e

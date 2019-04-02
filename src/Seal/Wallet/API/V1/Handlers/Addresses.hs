{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Seal.Wallet.API.V1.Handlers.Addresses where

import           Universum

import           Servant

import           Seal.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Seal.Wallet.WalletLayer as WalletLayer

import           Seal.Wallet.API.Request
import           Seal.Wallet.API.Response
import qualified Seal.Wallet.API.V1.Addresses as Addresses
import           Seal.Wallet.API.V1.Types


handlers :: PassiveWalletLayer IO -> ServerT Addresses.API Handler
handlers w =  listAddresses w
         :<|> newAddress w
         :<|> getAddress w
         :<|> importAddresses w

listAddresses :: PassiveWalletLayer IO
              -> RequestParams -> Handler (APIResponse [WalletAddress])
listAddresses pwl params = do
    addrs <- liftIO $ WalletLayer.getAddresses pwl params
    return $ fromSlice (rpPaginationParams params) addrs

newAddress :: PassiveWalletLayer IO
           -> NewAddress
           -> Handler (APIResponse WalletAddress)
newAddress pwl newAddressRequest = do
    res <- liftIO $ WalletLayer.createAddress pwl newAddressRequest
    case res of
         Left err      -> throwM err
         Right newAddr -> return $ single newAddr

-- | Validates an input 'Text' following these simple principles:
--
-- 1. The input text must be parseable into a Cardano Address;
-- 2. The input text must be a valid, @local@ 'Address', i.e. an 'Address'
--    known to this wallet.
getAddress :: PassiveWalletLayer IO
           -> Text
           -> Handler (APIResponse WalletAddress)
getAddress pwl addressRaw = do
    res <- liftIO $ WalletLayer.validateAddress pwl addressRaw
    case res of
         Left err   -> throwM err
         Right addr -> return $ single addr


importAddresses
    :: PassiveWalletLayer IO
    -> WalletId
    -> [WalAddress]
    -> Handler (APIResponse (BatchImportResult WalAddress))
importAddresses pwl walId addrs = do
    res <- liftIO $ WalletLayer.importAddresses pwl walId addrs
    case res of
        Left err   -> throwM err
        Right res' -> return $ single res'

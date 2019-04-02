{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Seal.Wallet.API.Request (
    RequestParams (..)
  -- * Handly re-exports
  , module Seal.Wallet.API.Request.Pagination
  , module Seal.Wallet.API.Request.Filter
  , module Seal.Wallet.API.Request.Sort
  ) where


import           Formatting (bprint, build, (%))
import           Seal.Infra.Util.LogSafe (BuildableSafeGen (..),
                     deriveSafeBuildable)

import           Seal.Wallet.API.Request.Filter
import           Seal.Wallet.API.Request.Pagination (PaginationMetadata (..),
                     PaginationParams)
import           Seal.Wallet.API.Request.Sort

data RequestParams = RequestParams
    { rpPaginationParams :: PaginationParams
    -- ^ The pagination-related parameters
    }

deriveSafeBuildable ''RequestParams
instance BuildableSafeGen RequestParams where
    buildSafeGen _sl RequestParams{..} =
        bprint ("pagination: "%build) rpPaginationParams

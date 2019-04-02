{-# LANGUAGE GADTs #-}

-- | Transaction metadata conform the wallet specification
module Seal.Wallet.Kernel.DB.TxMeta.Types (
    -- * Transaction metadata
    TxMeta(..)

    -- ** Lenses
  , txMetaId
  , txMetaAmount
  , txMetaInputs
  , txMetaOutputs
  , txMetaGoldInputs
  , txMetaGoldOutputs
  , txMetaDollarInputs
  , txMetaDollarOutputs
  , txMetaAccountInputs
  , txMetaAccountOutputs
  , txMetaCmds
  , txMetaCreationAt
  , txMetaIsLocal
  , txMetaIsOutgoing
  , txMetaWalletId
  , txMetaAccountIx

  -- * Transaction storage
  , MetaDBHandle (..)

  -- * Filtering and sorting primitives
  , AccountFops (..)
  , FilterOperation (..)
  , FilterOrdering (..)
  , Limit (..)
  , Offset (..)
  , Sorting (..)
  , SortCriteria (..)
  , SortDirection (..)

  -- * Domain-specific errors
  , TxMetaStorageError (..)
  , InvariantViolation (..)

  -- * Strict & lenient equalities
  , exactlyEqualTo
  , isomorphicTo
  , txIdIsomorphic

  -- * Internals useful for testing
  , uniqueElements
  , PutReturn (..)
  ) where

import           Universum

import           Control.Lens.TH (makeLenses)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           Formatting (bprint, build, int, later, shown, string, (%))
import qualified Formatting.Buildable
import           Test.QuickCheck (Arbitrary (..), Gen)

import           Seal.Wallet.Util (buildIndent, buildList, buildTrunc)
import qualified Seal.Chain.Txp as Txp
import qualified Seal.Core as Core

import           Seal.Wallet.Kernel.DB.HdRootId (HdRootId)

import           Test.Seal.Core.Arbitrary ()

{-------------------------------------------------------------------------------
  Transaction metadata
-------------------------------------------------------------------------------}

-- | Transaction metadata

--
-- NOTE: This does /not/ live in the acid-state database (and consequently
-- does not need a 'SafeCopy' instance), because this will grow without bound.
data TxMeta = TxMeta {
      -- | Transaction ID
      _txMetaId             :: Txp.TxId

      -- | Total amount
    , _txMetaAmount         :: Core.CoinGroup

      -- | Transaction inputs
    , _txMetaInputs         :: [(Txp.TxId, Word32, Core.Address, Core.Coin, Maybe Core.SlotId)]

      -- | Transaction outputs
    , _txMetaOutputs        :: [(Core.Address, Core.Coin, Maybe Core.SlotId)]
      
      -- | Seal
    , _txMetaGoldInputs     :: [(Txp.TxId, Word32, Core.Address, Core.GoldCoin)]
    , _txMetaGoldOutputs    :: [(Core.Address, Core.GoldCoin)]
    , _txMetaDollarInputs   :: [(Txp.TxId, Word32, Core.Address, Core.GoldDollar)]
    , _txMetaDollarOutputs  :: [(Core.Address, Core.GoldDollar)]
    , _txMetaAccountInputs  :: [(Core.Address, Txp.Nonce, Core.CoinGroup)]
    , _txMetaAccountOutputs :: [(Core.Address, Core.CoinGroup)]
    , _txMetaCmds           :: [Core.Cmd]

    -- | Transaction creation time
    , _txMetaCreationAt     :: Core.Timestamp

      -- | Is this a local transaction?
      --
      -- A transaction is local when /all/ of its inputs and outputs are
      -- to and from addresses owned by this wallet.
    , _txMetaIsLocal        :: Bool

      -- | Is this an outgoing transaction?
      --
      -- A transaction is outgoing when it decreases the wallet's balance.
    , _txMetaIsOutgoing     :: Bool

      -- The Wallet that added this Tx.
    , _txMetaWalletId       :: HdRootId

      -- The account index that added this Tx
    , _txMetaAccountIx      :: Word32
    } deriving Show

makeLenses ''TxMeta

instance Buildable TxMeta where
    build txMeta = bprint
        ( "TxMeta (#"%build%" "%string%" "%string%" "%build%"μs)"
        % "\n  amount: "%build
        % "\n  owner: "%buildTrunc build%"/"%build
        % "\n  inputs:         \n"%buildIndent 4 (buildList buildMetaInput)
        % "\n  outputs:        \n"%buildIndent 4 (buildList buildMetaOutput)
        % "\n  goldinputs:     \n"%buildIndent 4 (buildList buildMetaGoldInput)
        % "\n  goldoutputs:    \n"%buildIndent 4 (buildList buildMetaGoldOutput)
        % "\n  dollarinputs:   \n"%buildIndent 4 (buildList buildMetaDollarInput)
        % "\n  dollaroutputs:  \n"%buildIndent 4 (buildList buildMetaDollarOutput)
        % "\n  accountinputs:  \n"%buildIndent 4 (buildList buildMetaAccountInput)
        % "\n  accountoutputs: \n"%buildIndent 4 (buildList buildMetaAccountOutput)
        % "\n  cmds:           \n"%buildIndent 4 (buildList buildMetaCmd)
        )
        (txMeta ^. txMetaId)
        (if txMeta ^. txMetaIsLocal then "local" else "!local")
        (if txMeta ^. txMetaIsOutgoing then "out" else "in")
        (txMeta ^. txMetaCreationAt)
        (txMeta ^. txMetaAmount)
        (txMeta ^. txMetaWalletId)
        (txMeta ^. txMetaAccountIx)
        (txMeta ^. txMetaInputs)
        (txMeta ^. txMetaOutputs)
        (txMeta ^. txMetaGoldInputs)
        (txMeta ^. txMetaGoldOutputs)
        (txMeta ^. txMetaDollarInputs)
        (txMeta ^. txMetaDollarOutputs)
        (txMeta ^. txMetaAccountInputs)
        (txMeta ^. txMetaAccountOutputs)
        (txMeta ^. txMetaCmds)
      where
        buildMetaInput = later $ \(txid, i, addr, coin, lock) ->
            bprint (build%"."%int%" ~ "%buildTrunc build%" "%build%" "%build)
                txid
                i
                addr
                coin
                lock

        buildMetaOutput = later $ \(addr, coin, lock) ->
            bprint (buildTrunc build%" "%build%" "%build)
                addr
                coin
                lock

        buildMetaGoldInput = later $ \(txid, i, addr, goldcoin) ->
            bprint (build%"."%int%" ~ "%buildTrunc build%" "%build)
                txid
                i
                addr
                goldcoin

        buildMetaGoldOutput = later $ \(addr, goldcoin) ->
            bprint (buildTrunc build%" "%build)
                addr
                goldcoin
          
        buildMetaDollarInput = later $ \(txid, i, addr, dollarcoin) ->
            bprint (build%"."%int%" ~ "%buildTrunc build%" "%build)
                txid
                i
                addr
                dollarcoin

        buildMetaDollarOutput = later $ \(addr, dollarcoin) ->
            bprint (buildTrunc build%" "%build)
                addr
                dollarcoin

        buildMetaAccountInput = later $ \(accoount, nonce, coingroup) ->
            bprint (buildTrunc build%" "%build%" "%build)
                accoount
                nonce
                coingroup

        buildMetaAccountOutput = later $ \(accoount, coingroup) ->
            bprint (buildTrunc build%" "%build)
                accoount
                coingroup

        buildMetaCmd = later $ \cmd ->
            bprint (buildTrunc build)
                cmd

instance Buildable [TxMeta] where
    build [] = "Empty Tx Metas"
    build xs = bprint ("TxMetas\n" % buildIndent 2 (buildList build)) xs


-- | Strict equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- exactly the same data, and inputs & outputs needs to appear in exactly
-- the same order.
exactlyEqualTo :: TxMeta -> TxMeta -> Bool
exactlyEqualTo t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , t1 ^. txMetaAmount == t2 ^. txMetaAmount
        , t1 ^. txMetaInputs  == t2 ^. txMetaInputs
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , t1 ^. txMetaGoldInputs  == t2 ^. txMetaGoldInputs
        , t1 ^. txMetaGoldOutputs == t2 ^. txMetaGoldOutputs
        , t1 ^. txMetaDollarInputs  == t2 ^. txMetaDollarInputs
        , t1 ^. txMetaDollarOutputs == t2 ^. txMetaDollarOutputs
        , t1 ^. txMetaAccountInputs  == t2 ^. txMetaAccountInputs
        , t1 ^. txMetaAccountOutputs == t2 ^. txMetaAccountOutputs
        , t1 ^. txMetaCmds  == t2 ^. txMetaCmds
        , t1 ^. txMetaCreationAt == t2 ^. txMetaCreationAt
        , t1 ^. txMetaIsLocal == t2 ^. txMetaIsLocal
        , t1 ^. txMetaIsOutgoing == t2 ^. txMetaIsOutgoing
        , t1 ^. txMetaWalletId == t2 ^. txMetaWalletId
        , t1 ^. txMetaAccountIx == t2 ^. txMetaAccountIx
        ]

-- | Lenient equality for two 'TxMeta': two 'TxMeta' are equal if they have
-- the same data, same outputs in the same order and same inputs even if in different order.
-- NOTE: This check might be slightly expensive as it's nlogn in the
-- number of inputs, as it requires sorting.
isomorphicTo :: TxMeta -> TxMeta -> Bool
isomorphicTo t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , t1 ^. txMetaAmount == t2 ^. txMetaAmount
        , length (t1 ^. txMetaInputs)  == length (t2 ^. txMetaInputs)
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , length (t1 ^. txMetaGoldInputs)  == length (t2 ^. txMetaGoldInputs)
        , t1 ^. txMetaGoldOutputs == t2 ^. txMetaGoldOutputs
        , length (t1 ^. txMetaDollarInputs)  == length (t2 ^. txMetaDollarInputs)
        , t1 ^. txMetaDollarOutputs == t2 ^. txMetaDollarOutputs
        , length (t1 ^. txMetaAccountInputs)  == length (t2 ^. txMetaAccountInputs)
        , t1 ^. txMetaAccountOutputs == t2 ^. txMetaAccountOutputs
        , t1 ^. txMetaCmds  == t2 ^. txMetaCmds
        , t1 ^. txMetaCreationAt == t2 ^. txMetaCreationAt
        , t1 ^. txMetaIsLocal == t2 ^. txMetaIsLocal
        , t1 ^. txMetaIsOutgoing == t2 ^. txMetaIsOutgoing
        , t1 ^. txMetaWalletId == t2 ^. txMetaWalletId
        , t1 ^. txMetaAccountIx == t2 ^. txMetaAccountIx
        ]

-- This means TxMeta have same Inputs and TxId.
txIdIsomorphic :: TxMeta -> TxMeta -> Bool
txIdIsomorphic t1 t2 =
    and [ t1 ^. txMetaId == t2 ^. txMetaId
        , length (t1 ^. txMetaInputs)  == length (t2 ^. txMetaInputs)
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , length (t1 ^. txMetaInputs)  == length (t2 ^. txMetaInputs)
        , t1 ^. txMetaOutputs == t2 ^. txMetaOutputs
        , length (t1 ^. txMetaGoldInputs)  == length (t2 ^. txMetaGoldInputs)
        , t1 ^. txMetaGoldOutputs == t2 ^. txMetaGoldOutputs
        , length (t1 ^. txMetaDollarInputs)  == length (t2 ^. txMetaDollarInputs)
        , t1 ^. txMetaDollarOutputs == t2 ^. txMetaDollarOutputs
        , length (t1 ^. txMetaAccountInputs)  == length (t2 ^. txMetaAccountInputs)
        , t1 ^. txMetaAccountOutputs == t2 ^. txMetaAccountOutputs
        ]

type AccountIx = Word32
type WalletId = HdRootId
-- | Filter Operations on Accounts. This is hiererchical: you can`t have AccountIx without WalletId.
data AccountFops = Everything | AccountFops WalletId (Maybe AccountIx)

data InvariantViolation =
        TxIdInvariantViolated Txp.TxId
        -- ^ When attempting to insert a new 'MetaTx', the TxId
        -- identifying this transaction was already present in the storage,
        -- but with different values (i.e. different inputs/outputs etc)
        -- and this is effectively an invariant violation.
      | UndisputableLookupFailed Text
        -- ^ The db works in a try-catch style: it always first tries to
        -- insert data and if the PrimaryKey is already there, we catch the
        -- exception and do the lookup. This lookup should never fail, because
        -- the db is append only and if it`s found once, it should always
        -- be there.
      deriving Show

-- | A domain-specific collection of things which might go wrong when
-- storing & retrieving 'TxMeta' from a persistent storage.
data TxMetaStorageError =
      InvariantViolated InvariantViolation
    -- ^ One of the invariant was violated.
    | StorageFailure SomeException
    -- ^ The underlying storage failed to fulfill the request.
    deriving Show

instance Exception TxMetaStorageError

instance Buildable TxMetaStorageError where
    build storageErr = bprint shown storageErr

-- | Generates 'NonEmpty' collections which do not contain duplicates.
-- Limit the size to @size@ elements. @size@ should be > 0.
uniqueElements :: (Arbitrary a, Ord a) => Int -> Gen (NonEmpty a)
uniqueElements 0 = error "should have size > 0"
uniqueElements size = do
    (NonEmpty.fromList . Set.toList) <$> (go mempty)
    where
      go st = if (Set.size st == size)
        then return st
        else do
          a <- arbitrary
          go (Set.insert a st)


-- | Basic filtering & sorting types.

newtype Offset = Offset { getOffset :: Integer }

newtype Limit  = Limit  { getLimit  :: Integer }

data SortDirection =
      Ascending
    | Descending

data Sorting = Sorting {
      sbCriteria  :: SortCriteria
    , sbDirection :: SortDirection
    }

data SortCriteria =
      SortByCreationAt
    -- ^ Sort by the creation time of this 'Kernel.TxMeta'.
    | SortByAmountCoin
    | SortByAmountGoldCoin
    | SortByAmountGoldDollar
    -- ^ Sort the 'TxMeta' by the amount of money they hold.

data FilterOperation a =
    NoFilterOp
    -- ^ No filter operation provided
    | FilterByIndex a
    -- ^ Filter by index (e.g. equal to)
    | FilterByPredicate FilterOrdering a
    -- ^ Filter by predicate (e.g. lesser than, greater than, etc.)
    | FilterByRange a a
    -- ^ Filter by range, in the form [from,to]
    | FilterIn [a]
    deriving (Show, Eq)

data FilterOrdering =
      Equal
    | GreaterThan
    | GreaterThanEqual
    | LesserThan
    | LesserThanEqual
    deriving (Show, Eq, Enum, Bounded)

-- This is used mainly for testing and indicates, what happening
-- at the internals of SQlite during a putTxMetaT operation
-- @Tx@ means a new Tx was inserted
-- @Meta@ means the Tx was there but from a different Account, so a new TxMeta entry was created.
-- @No@ means the Tx was there from the same Account. This means nothing happens internally.
data PutReturn = Tx | Meta | No
    deriving (Show, Eq, Enum, Bounded)

instance Buildable PutReturn where
  build ret = bprint shown ret

-- | An opaque handle to the underlying storage, which can be easily instantiated
-- to a more concrete implementation like a Sqlite database, or even a pure
-- K-V store.
data MetaDBHandle = MetaDBHandle {
      closeMetaDB   :: IO ()
    , migrateMetaDB :: IO ()
    , clearMetaDB   :: IO ()
    , deleteTxMetas :: HdRootId -> Maybe Word32 -> IO ()
    , getTxMeta     :: Txp.TxId -> HdRootId -> Word32 -> IO (Maybe TxMeta)
    , putTxMeta     :: TxMeta -> IO ()
    , putTxMetaT    :: TxMeta -> IO PutReturn
    , getAllTxMetas :: IO [TxMeta]
    , getTxMetas    :: Offset -- Pagination: the starting offset of results.
                    -> Limit  -- An upper limit of the length of [TxMeta] returned.
                    -> AccountFops -- Filters on the Account. This may specidy an Account or a Wallet.
                    -> Maybe Core.Address -- Filters on the Addres.
                    -> FilterOperation Txp.TxId -- Filters on the TxId of the Tx.
                    -> FilterOperation Core.Timestamp -- Filters on the creation timestamp of the Tx.
                    -> Maybe Sorting -- Sorting of the results.
                    -> IO ([TxMeta], Maybe Int) -- the result in the form (results, totalEntries).
                                                -- totalEntries may be Nothing, because counting can
                                                -- be an expensive operation.
    }

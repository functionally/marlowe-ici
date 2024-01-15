{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.ICI.Indexer (
  runIndexer,
) where

import Codec.CBOR.Encoding (encodeMapLen, encodeString)
import Codec.CBOR.Write (toStrictByteString)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.State (StateT, gets, modify')
import Data.Default (Default (def))
import Data.IPLD.CID (CID)
import Language.Marlowe.Core.V1.Plate (extractNetworkAddresses)
import Language.Marlowe.Core.V1.Semantics (MarloweData (..), MarloweParams (rolesCurrency))
import Language.Marlowe.Core.V1.Semantics.Types.Address (
  serialiseAddress,
 )
import Language.Marlowe.ICI.Cbor (makeCid, toCid)
import Language.Marlowe.ICI.Ipfs (putCars, rename)
import Language.Marlowe.ICI.Ipld (encodeIpld)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (slotNo), SlotNo (SlotNo, unSlotNo), TxId, TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api (
  ContractId (ContractId),
  MarloweVersion (MarloweV1),
  Transaction (..),
  TransactionScriptOutput (TransactionScriptOutput, datum),
 )
import Language.Marlowe.Runtime.History.Api (
  CreateStep (CreateStep, createOutput),
  MarloweApplyInputsTransaction (..),
  MarloweBlock (..),
  MarloweCreateTransaction (..),
  MarloweWithdrawTransaction (..),
  SomeCreateStep (..),
 )
import System.IO (hPrint, hPutStrLn, stderr)

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Marlowe.ICI.PTree as PT
import qualified Language.Marlowe.ICI.PTree.Ipld as PT

data Index = Index
  { newCids :: M.Map CID BS.ByteString
  , contractIndex :: PT.PTree String CID
  , slotIndex :: PT.PTree String CID
  , policyIndex :: PT.PTree String CID
  , addressIndex :: PT.PTree String CID
  }

instance Default Index where
  def = Index def PT.Empty PT.Empty PT.Empty PT.Empty

runIndexer
  :: (MonadIO m)
  => TChan (SlotNo, MarloweBlock)
  -> String
  -> Int
  -> Int
  -> StateT Index m ()
runIndexer channel ipnsKey chunkSize batchSize =
  do
    (tip, header) <- nextBlock channel
    n <- gets $ M.size . newCids
    let report = slotNo header >= tip
    when (report || n >= batchSize) $
      outputIndex ipnsKey chunkSize report header
    runIndexer channel ipnsKey chunkSize batchSize

outputIndex
  :: (MonadIO m)
  => String
  -> Int
  -> Bool
  -> BlockHeader
  -> StateT Index m ()
outputIndex ipnsKey chunkSize report header =
  do
    newCids' <- gets $ M.toList . newCids
    contractCbor <- gets $ PT.toCBOR chunkSize . contractIndex
    slotCbor <- gets $ PT.toCBOR chunkSize . slotIndex
    policyCbor <- gets $ PT.toCBOR chunkSize . policyIndex
    addressCbor <- gets $ PT.toCBOR chunkSize . addressIndex
    let headerCbor = encodeIpld header
        slotNo' = show . unSlotNo $ slotNo header
        status =
          either (hPutStrLn stderr . ("Slot " <>) . (slotNo' <>) . (": " <>))
            . const
            . hPutStrLn stderr
            $ "Slot " <> slotNo' <> ": " <> show (length newCids') <> " CIDs"
        rootCbor =
          mconcat
            [ encodeMapLen 5
            , encodeString "tip" <> makeCid (fst headerCbor)
            , encodeString "contracts" <> makeCid (fst $ head contractCbor)
            , encodeString "slots" <> makeCid (fst $ head slotCbor)
            , encodeString "rolePolicies" <> makeCid (fst $ head policyCbor)
            , encodeString "paymentKeys" <> makeCid (fst $ head addressCbor)
            ]
        rootCid = toCid rootCbor
    liftIO $
      if report
        then do
          either (hPutStrLn stderr . ("Slot " <>) . (slotNo' <>) . (": " <>)) (const $ pure ())
            =<< ( putCars $
                    [(rootCid, toStrictByteString rootCbor), headerCbor]
                      <> contractCbor
                      <> slotCbor
                      <> policyCbor
                      <> addressCbor
                      <> newCids'
                )
          rename ipnsKey "20s" ("/ipfs/" <> show rootCid)
            >>= either
              (hPrint stderr . ("Slot " <>) . (slotNo' <>) . (": " <>))
              ( const . hPutStrLn stderr $
                  "Slot " <> slotNo' <> ": published " <> show (length newCids') <> " CIDs at /ipfs/" <> show rootCid
              )
        else
          unless (null newCids') $
            status =<< putCars newCids'
    modify' $ \index -> index{newCids = mempty}

nextBlock
  :: (MonadIO m)
  => TChan (SlotNo, MarloweBlock)
  -> StateT Index m (SlotNo, BlockHeader)
nextBlock channel =
  do
    -- FIXME: Handle rollbacks.
    (tip, MarloweBlock{..}) <- liftIO . atomically $ readTChan channel
    forM_ createTransactions $ handleCreates blockHeader
    forM_ applyInputsTransactions $ handleApply blockHeader
    forM_ withdrawTransactions $ handleWithdraws blockHeader
    pure (tip, blockHeader)

handleCreates
  :: (MonadIO m)
  => BlockHeader
  -> MarloweCreateTransaction
  -> StateT Index m ()
handleCreates header MarloweCreateTransaction{..} =
  mapM_ (\(txIx, step) -> handleCreate header (ContractId TxOutRef{..}) step) $
    M.toList newContracts

handleCreate
  :: (MonadIO m)
  => BlockHeader
  -> ContractId
  -> SomeCreateStep
  -> StateT Index m ()
handleCreate header contractId step@(SomeCreateStep MarloweV1 CreateStep{createOutput = TransactionScriptOutput{datum = MarloweData{..}}}) =
  do
    updateContractIndex contractId `uncurry` encodeIpld step
    let (contractIdCid, contractIdBytes) = encodeIpld $ showContractId False contractId
        -- FIXME: These analyses do not account for merkleized continuations.
        policy = show $ rolesCurrency marloweParams
        addresses =
          (BS8.unpack . B16.encode . BS.take 28 . BS.tail . uncurry serialiseAddress)
            `S.map` extractNetworkAddresses (Just marloweState) marloweContract mempty
    modify' $
      \index@Index{..} ->
        index
          { newCids = M.insert contractIdCid contractIdBytes newCids
          , slotIndex = PT.insert (showSlotNo $ slotNo header) contractIdCid slotIndex
          , policyIndex = if null policy then policyIndex else PT.insert policy contractIdCid policyIndex
          , addressIndex = foldr (`PT.insert` contractIdCid) addressIndex $ S.toList addresses
          }

handleApply
  :: (MonadIO m)
  => BlockHeader
  -> MarloweApplyInputsTransaction
  -> StateT Index m ()
handleApply _header step =
  case step of
    MarloweApplyInputsTransaction MarloweV1 _ Transaction{contractId} ->
      void $ updateContractIndex contractId `uncurry` encodeIpld step

handleWithdraws
  :: (MonadIO m)
  => BlockHeader
  -> MarloweWithdrawTransaction
  -> StateT Index m ()
handleWithdraws header MarloweWithdrawTransaction{..} =
  mapM_ (uncurry $ handleWithdraw header consumingTx) $
    M.toList consumedPayouts

handleWithdraw
  :: (MonadIO m)
  => BlockHeader
  -> TxId
  -> ContractId
  -> S.Set TxOutRef
  -> StateT Index m ()
handleWithdraw _header txId contractId txOutRefs =
  void
    . uncurry (updateContractIndex contractId)
    . encodeIpld
    $ A.object
      [ "consumedPayouts" A..= S.toList txOutRefs
      , "consumingTx" A..= txId
      ]

updateContractIndex
  :: (Monad m)
  => ContractId
  -> CID
  -> BS.ByteString
  -> StateT Index m ()
updateContractIndex contractId cid bytes =
  modify' $
    \index@Index{..} ->
      index
        { newCids = M.insert cid bytes newCids
        , contractIndex = PT.insert (showContractId True contractId) cid contractIndex
        }

showContractId
  :: Bool
  -> ContractId
  -> String
showContractId pad contractId =
  let contractId' = init . tail . LBS8.unpack $ A.encode contractId
   in -- FIXME: Ensure that contract ID is a fixed size, as required by `PTree`.
      if pad
        then take 65 contractId' <> replicate (68 - length contractId') '0' <> drop 65 contractId'
        else contractId'

showSlotNo
  :: SlotNo -> String
showSlotNo SlotNo{unSlotNo} =
  let slot = show unSlotNo
   in replicate (10 - length slot) '0' <> slot

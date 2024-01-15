{-# LANGUAGE TupleSections #-}

module Language.Marlowe.ICI.BulkSync (
  makeBulkSync,
  runBulkSync,
) where

import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Marlowe (MarloweT)
import Data.Word (Word8)
import Language.Marlowe.Protocol.BulkSync.Client (
  ClientStIdle (..),
  ClientStNext (..),
  ClientStPoll (..),
  MarloweBulkSyncClient (MarloweBulkSyncClient),
 )
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (BlockHeader, slotNo), SlotNo)
import Language.Marlowe.Runtime.Client (runMarloweBulkSyncClient)
import Language.Marlowe.Runtime.History.Api (MarloweBlock (blockHeader))

makeBulkSync
  :: SlotNo
  -> Word8
  -> IO (TChan (SlotNo, MarloweBlock), MarloweT IO (), IO ())
makeBulkSync batchSlot pageSize =
  do
    channel <- newTChanIO
    stop <- newTVarIO False
    pure
      ( channel
      , runBulkSync pageSize channel stop batchSlot
      , atomically $ writeTVar stop True
      )

runBulkSync
  :: Word8
  -> TChan (SlotNo, MarloweBlock)
  -> TVar Bool
  -> SlotNo
  -> MarloweT IO ()
runBulkSync initialPageSize channel stop batchSlot =
  let next =
        ClientStNext
          { recvMsgRollForward = \blocks BlockHeader{slotNo = tip} ->
              do
                liftIO . atomically $ forM_ blocks $ writeTChan channel . (tip,)
                let currentSlot = slotNo . blockHeader $ last blocks
                    newPageSize = if currentSlot <= batchSlot then initialPageSize else 1
                pure $ SendMsgRequestNext newPageSize next
          , recvMsgRollBackward = \_ _ -> pure $ SendMsgRequestNext 1 next
          , recvMsgWait =
              do
                stop' <- liftIO $ readTVarIO stop
                pure $
                  if stop'
                    then SendMsgCancel $ SendMsgDone ()
                    else SendMsgPoll next
          }
   in runMarloweBulkSyncClient . MarloweBulkSyncClient . pure $
        SendMsgRequestNext initialPageSize next

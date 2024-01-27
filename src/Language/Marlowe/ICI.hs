module Language.Marlowe.ICI (run) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.State (runStateT)
import Data.Default (def)
import Data.String (fromString)
import Language.Marlowe.ICI.BulkSync (makeBulkSync)
import Language.Marlowe.ICI.Indexer (runIndexer)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)

run
  :: String
  -> Int
  -> String
  -> String
  -> Int
  -> Int
  -> SlotNo
  -> IO ()
run host port ipfsApi ipnsKey chunkSize batchSize batchSlot =
  do
    -- FIXME: Bulk sync blocks until a page is full, so we need fetch single pages
    --        in order to avoid blocking at the tip.
    (channel, startBulkSync, _) <- makeBulkSync batchSlot 100
    void . forkIO $ connectToMarloweRuntime (fromString host) (fromIntegral port) startBulkSync
    void $ runIndexer channel ipfsApi ipnsKey chunkSize batchSize `runStateT` def

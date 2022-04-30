

{-# LANGUAGE FlexibleContexts #-}


module Language.Marlowe.ICI (
  marloweChainIndex
) where


import Cardano.Api
import Control.Monad.Except
import Language.Marlowe.CLI.Sync
import Language.Marlowe.CLI.Sync.Types
import Language.Marlowe.CLI.Types


marloweChainIndex :: MonadError CliError m
                 => MonadIO m
                 => LocalNodeConnectInfo CardanoMode  -- ^ The local node connection.
                 -> Bool                              -- ^ Whether to continue processing when the tip is reached.
                 -> Maybe FilePath                    -- ^ The file to restore the chain point from and save it to.
                 -> m ()                              -- ^ Action for watching for potential Marlowe transactions.
marloweChainIndex connection continue pointFile =
  watchMarloweWithPrinter connection continue pointFile
    $ \e ->
      print e

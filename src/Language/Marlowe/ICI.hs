

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}


module Language.Marlowe.ICI (
  marloweChainIndex
) where


import Cardano.Api hiding (Address)
import Control.Monad.Except
import Data.Default (Default(..))
import Data.IORef
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import Language.Marlowe.CLI.Sync
import Language.Marlowe.CLI.Sync.Types
import Language.Marlowe.CLI.Types
import Language.Marlowe.Semantics (MarloweParams(..))
import Ledger.Tx.CardanoAPI (toCardanoScriptHash)
import Plutus.V1.Ledger.Api (Address(..), Credential(ScriptCredential))

import qualified Language.Marlowe.ICI.PTree as PT
import qualified Data.Map.Strict as M


marloweChainIndex :: MonadError CliError m
                 => MonadIO m
                 => LocalNodeConnectInfo CardanoMode  -- ^ The local node connection.
                 -> Bool                              -- ^ Whether to continue processing when the tip is reached.
                 -> Maybe FilePath                    -- ^ The file to restore the chain point from and save it to.
                 -> m ()                              -- ^ Action for watching for potential Marlowe transactions.
marloweChainIndex connection continue pointFile =
  do
    indicesRef <- liftIO $ newIORef def
    watchMarloweWithPrinter connection continue pointFile
      $ process indicesRef
    indices <- liftIO $ readIORef indicesRef
    liftIO $ PT.printPTree "" (addressIndex indices)


data IciIndices =
  IciIndices
  {
     addresses     :: M.Map ScriptHash ScriptHash
  ,  unspents      :: M.Map TxIn ScriptHash
  ,  currencyIndex :: PT.PTree String MarloweParams
  ,  addressIndex  :: PT.PTree String MarloweEvent
  }


instance Default IciIndices where
  def = IciIndices mempty mempty PT.Empty PT.Empty


process :: IORef IciIndices
        -> MarloweEvent
        -> IO ()
process indicesRef me@Parameters{..} =
  do
    indices@IciIndices{..} <- readIORef indicesRef
    let
      indices' =
        indices
          {
            addresses     = M.insert (fromMarloweAddress meApplicationAddress) (fromMarloweAddress meApplicationAddress)
                              $ M.insert (fromMarloweAddress mePayoutAddress) (fromMarloweAddress meApplicationAddress)
                                addresses
          , currencyIndex = PT.insert (show $ rolesCurrency meParams) meParams
                              currencyIndex
          , addressIndex  = PT.insert (showScriptHash $ fromMarloweAddress meApplicationAddress) me addressIndex
          }
    writeIORef indicesRef indices'
process indicesRef me@Transaction{..} =
  do
    indices@IciIndices{..} <- readIORef indicesRef
    let
      outUnspents = catMaybes $ fromMarloweOut <$> meOuts
      outAddresses = snd <$> outUnspents
      inAddresses = catMaybes $ (`M.lookup` unspents) . miTxIn <$> meIns
      allAddresses = nub . sort . catMaybes $ (`M.lookup` addresses) <$> (inAddresses <> outAddresses)
      update ai address = PT.insert (showScriptHash address) me ai
      indices' =
        indices
          {
            unspents     = M.union unspents $ M.fromList outUnspents
          , addressIndex = foldl update addressIndex allAddresses
          }
    writeIORef indicesRef indices'
process _ _ = pure ()


fromMarloweOut :: MarloweOut
               -> Maybe (TxIn, ScriptHash)
fromMarloweOut ApplicationOut{..} = (moTxIn, ) <$> fromPlutusAddress moAddress
fromMarloweOut PayoutOut{..}      = (moTxIn, ) <$> fromPlutusAddress moAddress
fromMarloweOut _                  = Nothing


fromMarloweAddress :: MarloweAddress
                   -> ScriptHash
fromMarloweAddress (ApplicationCredential credential) = credential
fromMarloweAddress (PayoutCredential      credential) = credential


fromPlutusAddress :: Address
                  -> Maybe ScriptHash
fromPlutusAddress (Address (ScriptCredential credential) _ ) = either (const Nothing) Just $ toCardanoScriptHash credential
fromPlutusAddress _ = Nothing


showScriptHash :: ScriptHash
               -> String
showScriptHash = init . tail . show

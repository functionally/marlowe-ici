

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Language.Marlowe.ICI (
  marloweChainIndex
) where


import Cardano.Api hiding (Address)
import Control.Monad.Except
import Codec.CBOR.Encoding
import Codec.CBOR.Write
import Data.Aeson (toJSON)
import Data.Default (Default(..))
import Data.IPLD.CID (CID)
import Data.IORef
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import Language.Marlowe.CLI.Sync
import Language.Marlowe.CLI.Sync.Types
import Language.Marlowe.CLI.Types
import Language.Marlowe.ICI.Ipld (encodeIpld)
import Language.Marlowe.ICI.Ipfs (publish, putCars)
import Language.Marlowe.ICI.Cbor (makeCid, toCid)
import Language.Marlowe.Semantics (MarloweParams(..))
import Ledger.Tx.CardanoAPI (toCardanoScriptHash)
import Plutus.V1.Ledger.Api (Address(..), Credential(ScriptCredential))
import System.IO (hPrint, stderr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict as M
import qualified Language.Marlowe.ICI.PTree as PT
import qualified Language.Marlowe.ICI.PTree.Ipld as PT


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
--  liftIO $ output indicesRef


data IciIndices =
  IciIndices
  {
     addresses     :: M.Map ScriptHash ScriptHash
  ,  unspents      :: M.Map TxIn ScriptHash
  ,  block         :: BlockHeader
  ,  currencyIndex :: PT.PTree String CID
  ,  addressIndex  :: PT.PTree String CID
  ,  newCids       :: M.Map CID BS.ByteString
  }


instance Default IciIndices where
  def = IciIndices mempty mempty undefined PT.Empty PT.Empty mempty


process :: IORef IciIndices
        -> MarloweEvent
        -> IO ()
process indicesRef me@Parameters{..} =
  do
    indices@IciIndices{..} <- readIORef indicesRef
    let
      (eventCid, eventBytes) = encodeIpld me
      (addressCid, addressBytes) = encodeIpld meApplicationAddress
      indices' =
        indices
          {
            addresses     = M.insert (fromMarloweAddress meApplicationAddress) (fromMarloweAddress meApplicationAddress)
                              $ M.insert (fromMarloweAddress mePayoutAddress) (fromMarloweAddress meApplicationAddress)
                                addresses
          , currencyIndex = PT.insert (show $ rolesCurrency meParams) addressCid
                              currencyIndex
          , addressIndex  = PT.insert (showScriptHash $ fromMarloweAddress meApplicationAddress) eventCid addressIndex
          , newCids       = M.insert eventCid eventBytes
                              $ M.insert addressCid addressBytes
                                newCids
          }
    writeIORef indicesRef indices'
process indicesRef me@Transaction{..} =
  do
    indices@IciIndices{..} <- readIORef indicesRef
    let
      (eventCid, eventBytes) = encodeIpld me
      outUnspents = catMaybes $ fromMarloweOut <$> meOuts
      outAddresses = snd <$> outUnspents
      inAddresses = catMaybes $ (`M.lookup` unspents) . miTxIn <$> meIns
      allAddresses = nub . sort . catMaybes $ (`M.lookup` addresses) <$> (inAddresses <> outAddresses)
      update ai address = PT.insert (showScriptHash address) eventCid ai
      indices' =
        indices
          {
            unspents     = M.union unspents $ M.fromList outUnspents
          , addressIndex = foldl update addressIndex allAddresses
          , newCids      = M.insert eventCid eventBytes newCids
          , block        = meBlock
          }
    writeIORef indicesRef indices'
    output indicesRef
process _ _ = pure ()


output :: IORef IciIndices
       -> IO ()
output indicesRef =
  do
    indices@IciIndices{..} <- readIORef indicesRef
    let
      indices' = indices {newCids = mempty}
      currencyCbor = PT.toCBOR 1000 currencyIndex
      addressCbor = PT.toCBOR 1000 addressIndex
      blockCbor = encodeIpld block
      root =
        mconcat
          [
            encodeMapLen 3
          , encodeString "block"      <> makeCid (fst blockCbor)
          , encodeString "currencies" <> makeCid (fst $ head currencyCbor)
          , encodeString "addresses"  <> makeCid (fst $ head addressCbor )
          ]
      rootCid = toCid root
    writeIORef indicesRef indices'
    result <- putCars
      $ [(rootCid, toStrictByteString root), blockCbor]
      <> currencyCbor
      <> addressCbor
      <> M.toList newCids
    either (hPrint stderr) (const $ pure()) result
    result' <- publish "marlowe-ici" . LBS8.pack $ show rootCid <> "\n"
    either (hPrint stderr) (const $ pure()) result'
    putStrLn $ show rootCid <> "\t" <> show (toJSON block)


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

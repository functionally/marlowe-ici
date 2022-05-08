
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Language.Marlowe.ICI.Cbor (
  toCAR
, toCid
, cidFromString
, makeCid
) where


import Data.IPLD.CID (CID, Codec(DagCbor), buildCid, cidFromText, newCidV1)
import Data.Binary.ULEB128
import Data.Binary.Put (runPut)
import Codec.CBOR.Encoding
import Codec.CBOR.Write

import qualified Crypto.Hash as Crypto
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString            as BS   (ByteString, singleton)
import qualified Data.ByteString.Lazy       as LBS  (ByteString, length, fromStrict, toStrict)
import qualified Data.Text as T


toCAR :: [(CID, BS.ByteString)]
      -> LBS.ByteString
toCAR cbs =
  let
    carHeader =
      toLazyByteString
        $ mconcat
        [
          encodeMapLen 2
        , encodeString "roots" <> encodeListLen 1 <> makeCid (fst $ head cbs)
        , encodeString "version" <> encodeInteger 1
        ]
    carHeaderLength = fromIntegral $ LBS.length carHeader
    carData =
      runPut (putNatural carHeaderLength) <> carHeader
        <> mconcat
        [
          runPut (putNatural . fromIntegral $ LBS.length bytes') <> bytes'
        |
          (cid, bytes) <- cbs
        , let bytes' = makeCid' cid <> LBS.fromStrict bytes
        ]
  in
    carData


toCid :: Encoding -> CID
toCid =
  newCidV1 DagCbor
    . (Crypto.hash :: BS.ByteString -> Crypto.Digest Crypto.SHA256)
    . toStrictByteString

makeCid :: CID
         -> Encoding
makeCid =
  (encodeTag 42 <>)
    . encodeBytes
    . (BS.singleton 0x00 <>)
    . LBS.toStrict
    . makeCid'

makeCid' :: CID
         -> LBS.ByteString
makeCid' =
  Builder.toLazyByteString
   . buildCid


cidFromString :: String
              -> CID
cidFromString value =
  let
    Right cid = cidFromText $ T.pack value
  in
    cid

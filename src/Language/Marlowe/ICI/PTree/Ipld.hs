
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Language.Marlowe.ICI.PTree.Ipld (
  toCBOR
) where


import Data.IPLD.CID (CID, Codec(DagCbor), newCidV1)
import Data.List.Split (chunksOf)
import Language.Marlowe.ICI.Cbor
import Language.Marlowe.ICI.PTree
import Codec.CBOR.Encoding
import Codec.CBOR.Write

import qualified Crypto.Hash as Crypto
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text as T


toCBOR :: Int
       -> PTree String CID
       -> [(CID, BS.ByteString)]
toCBOR _ Empty = []
toCBOR chunkSize (Leaf vs) =
  let
    vss = chunksOf chunkSize $ reverse vs
    pack vs' next =
      let
        bytes =
          toStrictByteString
            . mconcat
            $ maybe (encodeMapLen 1) ((encodeMapLen 2 <>) . (encodeString "continued" <>) . makeCid) next
            : encodeString "items"
            : encodeListLen (fromIntegral $ length vs')
            : fmap makeCid vs'
        cid =
          newCidV1 DagCbor
            $ (Crypto.hash :: BS.ByteString -> Crypto.Digest Crypto.SHA3_256) bytes
      in
        (cid, bytes)
  in
    foldl (
      \next vs' ->
        case next of
          []                 -> [pack vs' Nothing]
          cbs@((cid, _) : _) -> pack vs' (Just cid) : cbs
    ) [] vss
toCBOR chunkSize (Node tree) =
  let
    children = M.map (toCBOR chunkSize) tree
    bytes =
      toStrictByteString
        $ encodeMapLen (fromIntegral $ M.size tree)
        <> mconcat
        [
          encodeString (T.pack key) <> makeCid cid'
        |
          (key, (cid', _) : _) <- M.assocs children
        ]
    cid =
      newCidV1 DagCbor
        $ (Crypto.hash :: BS.ByteString -> Crypto.Digest Crypto.SHA3_256) bytes
  in
    (cid, bytes)
      : concat (M.elems children)

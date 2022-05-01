module Language.Marlowe.ICI.Ipld (
  encodeIpld
) where
  

import Data.Aeson (ToJSON(..))
import Data.IPLD.CID (CID)
import Codec.CBOR.JSON (encodeValue)
import Codec.CBOR.Write (toStrictByteString)
import Language.Marlowe.ICI.Cbor (toCid)

import qualified Data.ByteString as BS


encodeIpld :: ToJSON a
           => a
           -> (CID, BS.ByteString)
encodeIpld x =
  let
    encoded = encodeValue $ toJSON x
  in
    (toCid encoded, toStrictByteString encoded)

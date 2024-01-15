module Language.Marlowe.ICI.Ipld (
  encodeIpld,
) where

import Codec.CBOR.Encoding (encodeString)
import Codec.CBOR.FlatTerm (TermToken (..), toFlatTerm)
import Codec.CBOR.JSON (encodeValue)
import Codec.CBOR.Write (toStrictByteString)
import Data.Aeson (ToJSON (..), encode)
import Data.IPLD.CID (CID)
import Language.Marlowe.ICI.Cbor (toCid)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T

encodeIpld
  :: (ToJSON a)
  => a
  -> (CID, BS.ByteString)
encodeIpld x =
  let encoded = encodeValue $ toJSON x
      encoded' = encodeString . T.pack . LBS8.unpack . encode $ toJSON x
      badIpldCborTag (TkInteger i) = i >= 2 ^ (64 :: Int)
      badIpldCborTag _ = False
   in -- FIXME: Is there a better way to handle this situation?
      if any badIpldCborTag $ toFlatTerm encoded
        then (toCid encoded', toStrictByteString encoded')
        else (toCid encoded, toStrictByteString encoded)

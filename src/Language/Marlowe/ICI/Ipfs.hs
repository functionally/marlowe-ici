
module Language.Marlowe.ICI.Ipfs (
  putCars
, ipfsRun
) where


import Cardano.Api               (AsType, HasTextEnvelope, deserialiseFromTextEnvelope, serialiseToTextEnvelope)
import Control.Monad             (unless, void, when)
import Data.Aeson                (FromJSON, ToJSON, decodeFileStrict, decodeStrict, encode)
import Data.IPLD.CID (CID)
import Data.String               (fromString)
import Development.Shake.Command (CmdOption(..), Exit(..), Stderr(..), Stdout(..), cmd)
import Language.Marlowe.ICI.Cbor (toCAR)
import System.Exit               (ExitCode(..))
import System.FilePath           (takeDirectory)

import qualified Data.ByteString           as BS (ByteString)
import qualified Data.ByteString.Char8     as BS8 (pack)
import qualified Data.ByteString.Lazy      as LBS (ByteString, writeFile)
import qualified System.Directory          as FS (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import qualified System.PosixCompat.Files  as FS (createLink)


putCars :: [(CID, BS.ByteString)]
        -> IO (Either String String)
putCars =
  ipfsRun ["dag", "import", "--pin-roots=false"]
    . Just
    . toCAR
  


ipfsRun :: [String]
        -> Maybe LBS.ByteString
        -> IO (Either String String)
ipfsRun arguments input =
  do
    (Exit code, Stdout result, Stderr msg) <-
      case input of
        Nothing     -> cmd "ipfs" arguments
        Just input' -> cmd (StdinBS input') "ipfs" arguments
    case code of
      ExitFailure _ -> pure $ Left msg
      ExitSuccess   -> pure $ Right result

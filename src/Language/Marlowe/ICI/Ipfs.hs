
module Language.Marlowe.ICI.Ipfs (
  putCars
, publish
, rename
, ipfsRun
, atalaSign
) where


import Cardano.Api               (AsType, HasTextEnvelope, deserialiseFromTextEnvelope, serialiseToTextEnvelope)
import Control.Monad             (unless, void, when)
import Data.Aeson                (FromJSON, ToJSON, Value, decodeFileStrict, decodeStrict, encode, encodeFile, object)
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
  

publish :: String
        -> LBS.ByteString
        -> IO (Either String String)
publish topic =
  ipfsRun ["pubsub", "pub", topic]
    . Just


rename :: String
       -> String
       -> IO (Either String String)
rename key value =
  ipfsRun ["name", "publish", "--key=" <> key, value] Nothing


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


atalaSign :: Value
          -> IO (Either String Value)
atalaSign credential =
  do
    encodeFile "credential.json" credential
    (Exit code, Stderr msg) <-
       cmd "java" ["-jar", "atala-prism.jar", "sign-credential", "identity.seed", "credential.json", "credential.signed"]
    result <- decodeFileStrict "credential.signed"
    case (code, result) of
      (ExitFailure _, _           ) -> pure $ Left msg
      (ExitSuccess  , Just result') -> pure $ Right result'
      _                             -> pure $ Left "Failed to decode JSON result."

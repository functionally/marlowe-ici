module Language.Marlowe.ICI.Ipfs (
  putCars,
  publish,
  rename,
  ipfsRun,
) where

import Data.IPLD.CID (CID)
import Development.Shake.Command (CmdOption (..), Exit (..), Stderr (..), Stdout (..), cmd)
import Language.Marlowe.ICI.Cbor (toCAR)
import System.Exit (ExitCode (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

putCars
  :: [(CID, BS.ByteString)]
  -> IO (Either String String)
putCars =
  ipfsRun ["dag", "import", "--pin-roots=false"]
    . Just
    . toCAR

publish
  :: String
  -> LBS.ByteString
  -> IO (Either String String)
publish topic =
  ipfsRun ["pubsub", "pub", topic]
    . Just

rename
  :: String
  -> String
  -> String
  -> IO (Either String String)
rename key ttl value =
  ipfsRun ["name", "publish", "--key=" <> key, "--ttl=" <> ttl, value] Nothing

ipfsRun
  :: [String]
  -> Maybe LBS.ByteString
  -> IO (Either String String)
ipfsRun arguments input =
  do
    (Exit code, Stdout result, Stderr msg) <-
      case input of
        Nothing -> cmd "ipfs" arguments
        Just input' -> cmd (StdinBS input') "ipfs" arguments
    case code of
      ExitFailure _ -> pure $ Left msg
      ExitSuccess -> pure $ Right result

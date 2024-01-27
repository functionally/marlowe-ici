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
  :: String
  -> [(CID, BS.ByteString)]
  -> IO (Either String String)
putCars ipfsApi =
  ipfsRun ipfsApi ["dag", "import", "--pin-roots=false"]
    . Just
    . toCAR

publish
  :: String
  -> String
  -> LBS.ByteString
  -> IO (Either String String)
publish ipfsApi topic =
  ipfsRun ipfsApi ["pubsub", "pub", topic]
    . Just

rename
  :: String
  -> String
  -> String
  -> String
  -> IO (Either String String)
rename ipfsApi key ttl value =
  ipfsRun ipfsApi ["name", "publish", "--key=" <> key, "--ttl=" <> ttl, value] Nothing

ipfsRun
  :: String
  -> [String]
  -> Maybe LBS.ByteString
  -> IO (Either String String)
ipfsRun ipfsApi arguments input =
  do
    let arguments' = ["--api", ipfsApi] <> arguments
    (Exit code, Stdout result, Stderr msg) <-
      case input of
        Nothing -> cmd "ipfs" arguments'
        Just input' -> cmd (StdinBS input') "ipfs" arguments'
    case code of
      ExitFailure _ -> pure $ Left msg
      ExitSuccess -> pure $ Right result



module Main (
  main
) where


import Cardano.Api
import Control.Monad.Except
import Language.Marlowe.CLI.Types
import Language.Marlowe.ICI
import System.Environment
import System.Exit
import System.IO


main :: IO ()
main =
  do
    [networkId, socketPath, continue, pointFile] <- getArgs
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = Testnet $ NetworkMagic $ read networkId
        , localNodeSocketPath      = socketPath
        }
    result <- runExceptT $ marloweChainIndex connection (read continue) (read pointFile)
    case result of
      Right ()      -> return ()
      Left  message -> do
                         hPutStrLn stderr $ unCliError message
                         exitFailure

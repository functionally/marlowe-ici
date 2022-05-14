
{-# LANGUAGE OverloadedStrings #-}


module Main (
  main
) where


import Cardano.Api
import Control.Monad.Except
import Language.Marlowe.Client (defaultMarloweParams)
import Language.Marlowe.CLI.Export (buildAddress)
import Language.Marlowe.CLI.Types
import Language.Marlowe.ICI
import System.Environment
import System.Exit
import System.IO

import qualified Data.Text.IO as T


main :: IO ()
main =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    T.hPutStrLn stderr
      $ "Version check: default Marlowe address = "
      <> serialiseAddress (buildAddress defaultMarloweParams (Testnet $ NetworkMagic 1564) NoStakeAddress :: AddressInEra AlonzoEra)
    [networkId, socketPath, continue, pointFile] <- getArgs
    let
      network = Testnet . NetworkMagic $ read networkId
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network
        , localNodeSocketPath      = socketPath
        }
    result <- runExceptT $ marloweChainIndex connection (read continue) (read pointFile)
    case result of
      Right ()      -> return ()
      Left  message -> do
                         hPutStrLn stderr $ unCliError message
                         exitFailure

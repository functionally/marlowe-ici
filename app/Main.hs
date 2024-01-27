{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import Data.Version (showVersion)
import Language.Marlowe.ICI (run)
import Paths_marlowe_ici (version)

import qualified Options.Applicative as O

main :: IO ()
main =
  do
    Command{..} <- O.execParser commandParser
    run host port ipfsApi ipnsKey chunkSize batchSize $ fromInteger batchSlot

data Command = Command
  { host :: String
  , port :: Int
  , ipfsApi :: String
  , ipnsKey :: String
  , chunkSize :: Int
  , batchSize :: Int
  , batchSlot :: Integer
  }
  deriving (Show)

commandParser :: O.ParserInfo Command
commandParser =
  let commandOptions =
        Command
          <$> O.strOption
            (O.long "host" <> O.value "localhost" <> O.metavar "HOST" <> O.help "Host for Marlowe proxy service." <> O.showDefault)
          <*> O.option
            O.auto
            (O.long "port" <> O.value 3700 <> O.metavar "PORT" <> O.help "Port for Marlowe proxy service." <> O.showDefault)
          <*> O.strOption (O.long "ipfs-api" <> O.metavar "MULTIADDR" <> O.help "The multiaddr for the IPFS API.")
          <*> O.strOption (O.long "ipns-key" <> O.metavar "KEY_NAME" <> O.help "The name of the IPNS key for publishing.")
          <*> O.option
            O.auto
            ( O.long "chunk-size"
                <> O.value 1000
                <> O.metavar "INTEGER"
                <> O.help "The chunk size for IPFS CAR files."
                <> O.showDefault
            )
          <*> O.option
            O.auto
            ( O.long "batch-size"
                <> O.value 1000
                <> O.metavar "INTEGER"
                <> O.help "The batch size for the initial IPLD puts."
                <> O.showDefault
            )
          <*> O.option
            O.auto
            ( O.long "batch-slot"
                <> O.value 35000000
                <> O.metavar "SLOT_NO"
                <> O.help "The last slot for batching IPLD puts."
                <> O.showDefault
            )
   in O.info
        ( O.helper
            <*> (O.infoOption ("marlowe-ici " <> showVersion version) $ O.long "version" <> O.help "Show version")
            <*> commandOptions
        )
        ( O.fullDesc
            <> O.progDesc "This command-line tool builds and publishes IPLD indexes of Marlowe contracts and transactions."
            <> O.header "marlowe-ici : build IPLD indexes for Marlowe"
        )

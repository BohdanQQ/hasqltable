module Config
    ( Config(..)
    , parseConfig
    ) where

import           Data.Complex
import           Options.Applicative

data Config = Config
    { file      :: String
    , schema    :: String
    , delimiter :: String
    }

parseConfig :: Parser Config
parseConfig =
    Config
        <$> strArgument (metavar "FILE")
        <*> strArgument (metavar "SCHEMA")
        <*> strOption (short 'd' <> long "delimiter" <> value ",")

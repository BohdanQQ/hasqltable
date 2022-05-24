module Config
    ( Config(..)
    , parseConfig
    ) where

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
        <*> strArgument (metavar "SCHEMA" <> help "A string of the b/i/d/s characters representing bool/int/double/string column value in the order as they appear (e.g. \"siddd\" means the first colum is string, second is int, etc.)")  
        <*> strOption (short 'd' <> long "delimiter" <> value ",")

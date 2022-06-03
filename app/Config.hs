module Config
    ( Config(..)
    , parseConfig
    , SchemaInput(..)
    ) where

import           Options.Applicative

data SchemaInput = Raw String | File String

data Config = Config
    { file       :: String
    , delimiter  :: String
    , schemaSpec :: SchemaInput
    }

parseConfig :: Parser Config
parseConfig =
    Config
        <$> strArgument (metavar "FILE")
        <*> strOption
                (short 'd' <> long "delimiter" <> value "," <> help
                    "The column delimiter"
                )
        <*> (   File
            <$> strOption
                    (short 's' <> long "fileSpec" <> help
                        "A file to be used to load the schema specifiaction"
                    )
            <|> Raw
            <$> strOption
                    (  short 'r'
                    <> long "rawSpec"
                    <> help
                           "A string of the b/i/d/s characters representing bool/int/double/string column value in the order as they appear (e.g. \"siddd\" means the first colum is string, second is int, etc.)"
                    )
            )


{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DuplicateRecordFields
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

module ClickHouse.Lock where

-- Internal
import ClickHaskell.Client (ChCredential (..), HttpChClient, Reading, initClient, interpretClient, setHttpClientTimeout)
import ClickHaskell.Generics (ReadableFrom)
import ClickHaskell.Tables (Table, Column)
import ClickHouse.DbTypes (ChString)


-- GHC included libraries
import Control.Applicative ((<|>))
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy.Char8 as BSL8 (putStrLn)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack, unpack)
import System.Environment (lookupEnv)


-- External dependencies
import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), encode, pairs, withObject, (.:))
import GHC.Generics (Generic)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, info, optional, progDesc, (<**>))
import Options.Applicative.Builder (help, long, short, strOption)
import Options.Applicative.Extra (helperWith)


cli :: ParserInfo (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
cli = info (
  (,,,)
    <$> (optional . strOption) (long "user" <> short 'u' <> help "username (default: \"default\")")
    <*> (optional . strOption) (long "password" <> help "user password (default: \"\")")
    <*> (optional . strOption) (long "host" <> short 'h' <> help "ClickHouse host (default: \"localhost:8123\"")
    <*> (optional . strOption) (long "database" <> short 'd' <> help "database (default: \"default\")")
  <**>
    helperWith (long "help" <> help "Show this help text")
  )
  (fullDesc
    <> progDesc "ToDo: Description"
    <> header "ToDo: Header"
  )

type SystemTables =
  Table
    "tables"
   '[ Column "database" ChString
    , Column "table" ChString
    ]

data ClickHouseColumns = MkClickHouseColumns
  { database :: StrictByteString
  , table :: StrictByteString
  }
  deriving (Generic, Show)

instance ReadableFrom SystemTables ClickHouseColumns

runClickHouseLock :: IO ()
runClickHouseLock = do
  (maybeUserCli, maybePasswordCli, maybeHostCli, maybeDatabaseCli) <- execParser cli
  maybeUserEnv <- fmap T.pack <$> lookupEnv "CLICKHOUSE_USER"
  maybePasswordEnv <- fmap T.pack <$> lookupEnv "CLICKHOUSE_PASSWORD"
  maybeHostEnv <- fmap T.pack <$> lookupEnv "CLICKHOUSE_HOST"
  maybeDatabaseEnv <- fmap T.pack <$> lookupEnv "CLICKHOUSE_DATABASE"


  let chCredential = MkChCredential
        { chLogin = fromMaybe "default" (maybeUserCli <|> maybeUserEnv)
        , chPass = fromMaybe "" (maybePasswordCli <|> maybePasswordEnv)
        , chUrl = fromMaybe "http://localhost:8123" (maybeHostCli <|> maybeHostEnv)
        , chDatabase = fromMaybe "default" (maybeDatabaseCli <|> maybeDatabaseEnv)
        }

  client <- initClient
    @HttpChClient
    chCredential
    (Just $ setHttpClientTimeout 500_000)

  res <- interpretClient
    @(Reading ClickHouseColumns -> SystemTables)
    client

  print res

example :: IO ()
example =
  BSL8.putStrLn $ encode
    [ Table
      { name="example"
      , columns = 
        [ MkLockedColumn "column1" "String"
        , MkLockedColumn "column2" "Int64"
        ]
      }
    , View
      { name = "exapleView"
      , columns =
        [ MkLockedColumn "column1" "String"
        , MkLockedColumn "column2" "Int64"
        ]
      , parameters =
        [ MkLockedParameter "parameter1" "String"
        ]
      }
    ]




data LockPart =
  Table
    { name :: Text
    , columns :: [LockedColumn]
    }
  |
  View
    { name :: Text
    , columns :: [LockedColumn]
    , parameters :: [LockedParameter]
    }
  deriving (Generic)

instance ToJSON LockPart where
  toEncoding Table{name, columns} =
      pairs
        (  "type" .= ("table" :: Text)
        <> "name" .= name
        <> "columns" .= columns
        )
  toEncoding View{name, columns, parameters} =
    pairs
      (  "type" .= ("view" :: Text)
      <> "name" .= name
      <> "columns" .= columns
      <> "parameters" .= parameters
      )

instance FromJSON LockPart where
  parseJSON = withObject "LockPart" $ \v -> do
    columnType <- v .: "type"
    case columnType :: Text of
      "table" -> Table
        <$> v .: "name"
        <*> v .: "columns"
      "view" -> View
        <$> v .: "name"
        <*> v .: "columns"
        <*> v .: "parameters"
      unsupportedType -> fail ("unsupported lock part type: " <> T.unpack unsupportedType)




data LockedColumn = MkLockedColumn
  { name :: Text
  , columnType :: Text
  }
  deriving (Generic)

instance FromJSON LockedColumn where
  parseJSON = withObject "LockedColumn" $ \v -> do
    MkLockedColumn
      <$> v .: "name"
      <*> v .: "type"

instance ToJSON LockedColumn where
  toEncoding MkLockedColumn{name, columnType} =
    pairs
      (  "name" .= name
      <> "type" .= columnType
      )




data LockedParameter = MkLockedParameter
  { name :: Text
  , parameterType :: Text
  }
  deriving (Generic)

instance FromJSON LockedParameter where
  parseJSON = withObject "LockerParameter" $ \v -> do
    MkLockedParameter
      <$> v .: "name"
      <*> v .: "type"

instance ToJSON LockedParameter where
  toEncoding MkLockedParameter{name, parameterType} =
    pairs
      (  "name" .= name
      <> "type" .= parameterType
      )

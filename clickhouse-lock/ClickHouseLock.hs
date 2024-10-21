{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DuplicateRecordFields
  , InstanceSigs
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}

module ClickHouseLock where

-- Internal
import ClickHaskell.Client (ChCredential (..), ReadableFrom, select)
import ClickHaskell.DbTypes (ChString)
import ClickHaskell.Tables (Column)


-- GHC included libraries
import Control.Applicative ((<|>))
import Data.ByteString (StrictByteString)
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy as BSL (writeFile)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)


-- External dependencies
import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), encode, pairs, withObject, (.:))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative (ParserInfo, execParser, fullDesc, header, info, optional, progDesc, (<**>))
import Options.Applicative.Builder (help, long, short, strOption)
import Options.Applicative.Extra (helperWith)

main :: IO ()
main = runClickHouseLock

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

type TablesColumns =
 '[ Column "database" ChString
  , Column "engine" ChString
  , Column "table" ChString
  ]

data ClickHouseColumns = MkClickHouseColumns
  { database :: StrictByteString
  , engine :: StrictByteString
  , table :: StrictByteString
  }
  deriving (Generic, ReadableFrom TablesColumns)

runClickHouseLock :: IO ()
runClickHouseLock = do
  (maybeCliUser, maybeCliPassword, maybeCliHost, maybeCliDatabase) <- execParser cli
  maybeEnvUser     <- fmap T.pack <$> lookupEnv "CLICKHOUSE_USER"
  maybeEnvPassword <- fmap T.pack <$> lookupEnv "CLICKHOUSE_PASSWORD"
  maybeEnvHost     <- fmap T.pack <$> lookupEnv "CLICKHOUSE_HOST"
  maybeEnvDatabase <- fmap T.pack <$> lookupEnv "CLICKHOUSE_DATABASE"


  let mDatabase = maybeCliDatabase <|> maybeEnvDatabase
      mHost     = maybeCliHost     <|> maybeEnvHost
      mPassword = maybeCliPassword <|> maybeEnvPassword
      mUser     = maybeCliUser     <|> maybeEnvUser
      chCredentials = MkChCredential
        { chLogin    = fromMaybe "default" mUser
        , chPass     = fromMaybe "" mPassword
        , chUrl      = fromMaybe "http://localhost:8123" mHost
        , chDatabase = fromMaybe "default" mDatabase
        }

  manager <- newManager defaultManagerSettings

  res <-
    select
      @TablesColumns
      @ClickHouseColumns
      manager
      chCredentials
      (
        " SELECT database, engine, table \
        \ FROM system.tables " <> fromMaybe ""
          ( (\dbName -> " WHERE database = '" <> dbName <> "'")
          . byteString . encodeUtf8
          <$> mDatabase
          )
      )

  BSL.writeFile "clickhouse-lock.json"
    . encode
    . map mkLockPart
    $ res




mkLockPart :: ClickHouseColumns -> LockPart
mkLockPart MkClickHouseColumns{engine, table} = case engine of
  "View" -> View{name=decodeUtf8 table, columns=[], parameters=[]}
  _      -> Table{name=decodeUtf8 table, columns=[]}




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


-- * Syntax

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




data LockedColumn = MkLockedColumn
  { name :: Text
  , columnType :: Text
  }
  deriving (Generic)




data LockedParameter = MkLockedParameter
  { name :: Text
  , parameterType :: Text
  }
  deriving (Generic)


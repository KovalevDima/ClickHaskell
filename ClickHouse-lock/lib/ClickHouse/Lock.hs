{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , DuplicateRecordFields
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , ScopedTypeVariables
#-}
module ClickHouse.Lock where

-- GHC included libraries
import Data.Text as T (Text, unpack)
import Data.ByteString.Lazy.Char8 as BSL8 (putStrLn)


-- External dependencies
import Data.Aeson (ToJSON(..), KeyValue((.=)), encode, pairs, (.:), withObject, FromJSON(..))
import GHC.Generics (Generic)


runClickHouseLock :: IO ()
runClickHouseLock = do
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

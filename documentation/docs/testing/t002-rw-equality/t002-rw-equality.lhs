1. Runs *insertInto* of a sample into the all supported types table
2. Runs *selectFrom* from the same table
3. Checks if result equals sample value

```haskell
{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , TypeApplications
  , NumericUnderscores
  , TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
#-}

module Main (main) where

-- Internal
import ClickHaskell

-- GHC included
import Control.Monad      (when)
import GHC.Generics       (Generic)
import Data.Fixed         (Fixed)
import Data.Time          (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)


main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  connOld <- openConnection (overrideMaxRevision 1 defaultConnectionArgs)
  
  t2 connection
  t2 connOld
  putStrLn "T2WriteReadEquality: Ok"

t2 :: Connection -> IO ()
t2 connection = do
  command connection "DROP TABLE IF EXISTS writeReadEqualityTable;"
  command connection createTableQuery

  insert
    (intoTable
      @"writeReadEqualityTable"
      @TestColumns
      @TestData
    )
    connection
    [testData, testData]

  [_result, result] <-
    concat <$>
      select
        (fromTable
          @"writeReadEqualityTable"
          @TestColumns
        )
        connection
        pure

  let testLabel = "WriteReadEquality: "

  (when (result /= testData) . error)
    (  testLabel <> "Unequal result.\n"
    <> "Writed data: " <> show testData <> "\n"
    <> "Readed data: " <> show result)

  print $ testLabel <> "Ok"


type TestColumns =
  '[ Column "dateTime" (DateTime "UTC")
   , Column "dateTimeAmsterdam" (DateTime "Europe/Amsterdam")
   , Column "dateTimeNullable" (Nullable (DateTime "UTC"))
   , Column "dateTime64p3" (DateTime64 3 "UTC")
   , Column "dateTime64p9" (DateTime64 9 "UTC")
   , Column "dateTime64Nullable" (Nullable (DateTime64 3 "UTC"))
   , Column "date" Date
   , Column "float32" Float32
   , Column "float64" Float64
   , Column "decimal32" (Decimal32 1 1)
   , Column "decimal64" (Decimal64 10 3)
   , Column "decimal128" (Decimal128 19 10)
   , Column "decimal256" (Decimal256 39 13)
   , Column "bool" Bool
   , Column "enum8" (Enum8 "'world' = -1, 'hello' = 1")
   , Column "enum16" (Enum16 "'world' = -1, 'hello' = 1")
   , Column "int128" Int128
   , Column "int128Nullable" (Nullable Int128)
   , Column "int16" Int16
   , Column "int16Nullable" (Nullable Int16)
   , Column "int32" Int32
   , Column "int32Nullable" (Nullable Int32)
   , Column "int64" Int64
   , Column "int64Nullable" (Nullable Int64)
   , Column "int256" Int256
   , Column "int256Nullable" (Nullable Int256)
   , Column "int8" Int8
   , Column "int8Nullable" (Nullable Int8)
   , Column "string" ChString
   , Column "stringNullable" (Nullable ChString)
   , Column "uint256" UInt256
   , Column "uint256Nullable" (Nullable UInt256)
   , Column "uint128" UInt128
   , Column "uint128Nullable" (Nullable UInt128)
   , Column "uint16" UInt16
   , Column "uint16Nullable" (Nullable UInt16)
   , Column "uint32" UInt32
   , Column "uint32Nullable" (Nullable UInt32)
   , Column "uint64" UInt64
   , Column "uint64Nullable" (Nullable UInt64)
   , Column "uint8" UInt8
   , Column "uint8Nullable" (Nullable UInt8)
   , Column "uuid" UUID
   , Column "uuidNullable" (Nullable UUID)
   , Column "stringArray" (Array ChString)
   , Column "int64Array" (Array Int64)
   , Column "int8Array" (Array Int8)
   ]

data TestData = MkTestData
  { dateTime :: UTCTime
  , dateTimeAmsterdam :: UTCTime
  , dateTimeNullable :: Nullable UTCTime
  , dateTime64p3 :: UTCTime
  , dateTime64p9 :: UTCTime
  , dateTime64Nullable :: Nullable UTCTime
  , date :: Date
  , float32 :: Float32
  , float64 :: Float64
  , decimal32 :: Fixed 10
  , decimal64 :: Fixed 1_000
  , decimal128 :: Fixed 10_000_000_000
  , decimal256 :: Fixed 10_000_000_000_000
  , bool :: Bool
  , enum8 :: Enum8 "'world' = -1, 'hello' = 1"
  , enum16 :: Enum16 "'world' = -1, 'hello' = 1"
  , int256 :: Int256
  , int256Nullable :: Nullable Int256
  , int128 :: Int128
  , int128Nullable :: Nullable Int128
  , int16 :: Int16
  , int16Nullable :: Nullable Int16
  , int32 :: Int32
  , int32Nullable :: Nullable Int32
  , int64 :: Int64
  , int64Nullable :: Nullable Int64
  , int8 :: Int8
  , int8Nullable :: Nullable Int8
  , string :: ChString
  , stringNullable :: Nullable ChString
  , uint256 :: UInt256
  , uint256Nullable :: Nullable UInt256
  , uint128 :: UInt128
  , uint128Nullable :: Nullable UInt128
  , uint16 :: UInt16
  , uint16Nullable :: Nullable UInt16
  , uint32 :: UInt32
  , uint32Nullable :: Nullable UInt32
  , uint64 :: UInt64
  , uint64Nullable :: Nullable UInt64
  , uint8 :: UInt8
  , uint8Nullable :: Nullable UInt8
  , uuid :: UUID
  , uuidNullable :: Nullable UUID
  , stringArray :: [ChString]
  , int64Array :: [Int64]
  , int8Array :: [Int8]
  }
  deriving (Generic, Show, Eq)

instance ClickHaskell TestColumns TestData

testData :: TestData
testData = MkTestData
  { dateTime = posixSecondsToUTCTime 0
  , dateTimeAmsterdam = posixSecondsToUTCTime 0
  , dateTimeNullable = Just (posixSecondsToUTCTime 42)
  , dateTime64p3 = posixSecondsToUTCTime 42.003
  , dateTime64p9 = posixSecondsToUTCTime 42.000000003
  , dateTime64Nullable = Just (posixSecondsToUTCTime 42)
  , date = 0
  , float32 = 42.42
  , float64 = 42.42
  , decimal32 = -10000.1
  , decimal64 = -10000.1
  , decimal128 = -10000.100000000001
  , decimal256 = -10000.1000000000000000000000001
  , bool = False
  , enum8 = 0
  , enum16 = 0
  , int128 = toChType (-128 :: Int128)
  , int128Nullable = toChType $ Just (-128 :: Int128)
  , int16 = toChType (-16 :: Int16)
  , int16Nullable = toChType $ Just (-16 :: Int16)
  , int32 = toChType (-32 :: Int32)
  , int32Nullable = toChType $ Just (-32 :: Int32)
  , int64 = toChType (-64 :: Int64)
  , int64Nullable = toChType $ Just (-64 :: Int64)
  , int256 = -64
  , int256Nullable = Just (-64)
  , int8 = toChType (-8 :: Int8)
  , int8Nullable = toChType $ Just (-8 :: Int8)
  , string = "string"
  , stringNullable = Just "string"
  , uint256 = 64
  , uint256Nullable = Just (-64)
  , uint128 = toChType (128 :: UInt128)
  , uint128Nullable = toChType $ Just (128 :: UInt128)
  , uint16 = 16
  , uint16Nullable = Just 16
  , uint32 = 32
  , uint32Nullable = Just 32
  , uint64 = 64
  , uint64Nullable = Just 64
  , uint8 = 8
  , uint8Nullable = Just 8
  , uuid = let pos = (^) @UInt64 @UInt64 16 in
      toChType (0 :: UInt64, (pos 3)*4 + (pos 2)*2  )
    -- ^ 00000000-0000-0000-0000-000000004200
  , uuidNullable = Nothing
  , stringArray = ["array1", "array2"]
  , int64Array = [64, 128]
  , int8Array = [0, 1, 2]
  }

createTableQuery :: Command
createTableQuery = 
  "CREATE TABLE IF NOT EXISTS writeReadEqualityTable \
  \( \
  \    `dateTime` DateTime('UTC'), \
  \    `dateTimeAmsterdam` DateTime('Europe/Amsterdam'), \
  \    `dateTime64p3` DateTime64(3, 'UTC'), \
  \    `dateTime64p9` DateTime64(9, 'UTC'), \
  \    `dateTimeNullable` Nullable(DateTime('UTC')), \
  \    `dateTime64Nullable` Nullable(DateTime64(3, 'UTC')), \
  \    `date` Date, \
  \    `float32` Float32, \
  \    `float64` Float64, \
  \    `decimal32` Decimal(1, 1), \
  \    `decimal64` Decimal(10, 3), \
  \    `decimal128` Decimal(19, 10), \
  \    `decimal256` Decimal(39, 13), \
  \    `bool` Bool, \
  \    `enum8` Enum8('hello'=1, 'world'=-1), \
  \    `enum16` Enum16('hello'=1, 'world'=-1), \
  \    `int256` Int256, \
  \    `int256Nullable` Nullable(Int256), \
  \    `int128` Int128, \
  \    `int128Nullable` Nullable(Int128), \
  \    `int16` Int16, \
  \    `int16Nullable` Nullable(Int16), \
  \    `int32` Int32, \
  \    `int32Nullable` Nullable(Int32), \
  \    `int64` Int64, \
  \    `int64Nullable` Nullable (Int64), \
  \    `int8` Int8, \
  \    `int8Nullable` Nullable(Int8), \
  \    `string` String, \
  \    `stringNullable` Nullable(String), \
  \    `uint256` UInt256, \
  \    `uint256Nullable` Nullable(UInt256), \
  \    `uint128` UInt128, \
  \    `uint128Nullable` Nullable(UInt128), \
  \    `uint16` UInt16, \
  \    `uint16Nullable` Nullable (UInt16), \
  \    `uint32` UInt32, \
  \    `uint32Nullable` Nullable(UInt32), \
  \    `uint64` UInt64, \
  \    `uint64Nullable` Nullable(UInt64), \
  \    `uint8` UInt8, \
  \    `uint8Nullable` Nullable(UInt8), \
  \    `uuid` UUID, \
  \    `uuidNullable` Nullable(UUID), \
  \    `stringArray` Array(String), \
  \    `int64Array` Array(Int64), \
  \    `int8Array` Array(Int8), \
  \) \
  \ENGINE = MergeTree \
  \PARTITION BY () \
  \ORDER BY ();"
```

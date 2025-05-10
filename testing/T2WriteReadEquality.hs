{-# LANGUAGE
    DataKinds
  , AllowAmbiguousTypes
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}

module T2WriteReadEquality
  ( t2
  ) where

-- Internal
import ClickHaskell
  ( ClickHaskell, selectFrom, insertInto
  , command
  , Connection
  , Table
  , Column
  , toChType
  , UInt8, UInt16, UInt32, UInt64, UInt128
  , UUID, DateTime, ChString, Int128, Word128
  , Nullable, DateTime
  )

-- GHC included
import Control.Monad      (when)
import Data.Int           (Int16, Int32, Int64, Int8)
import Data.Word          (Word16, Word32, Word64, Word8)
import GHC.Generics       (Generic)

t2 :: Connection -> IO ()
t2 connection = do
  command connection "TRUNCATE writeReadEqualityTable;"

  insertInto
    @TestTable
    @TestData
    connection
    [testData]

  [result] <-
    concat <$>
      selectFrom
        @TestTable
        @TestData
        connection
        pure

  let testLabel = "WriteReadEquality: "

  (when (result /= testData) . error)
    (  testLabel <> "Unequal result.\n"
    <> "Writed data: " <> show testData <> "\n"
    <> "Readed data: " <> show result)

  print $ testLabel <> "Ok"


type TestTable = Table "writeReadEqualityTable" TestColumns
type TestColumns =
  '[ Column "dateTime" (DateTime "UTC")
   , Column "dateTimeNullable" (Nullable (DateTime "UTC"))
   , Column "int128" Int128
   , Column "int128Nullable" (Nullable Int128)
   , Column "int16" Int16
   , Column "int16Nullable" (Nullable Int16)
   , Column "int32" Int32
   , Column "int32Nullable" (Nullable Int32)
   , Column "int64" Int64
   , Column "int64Nullable" (Nullable Int64)
   , Column "int8" Int8
   , Column "int8Nullable" (Nullable Int8)
   , Column "string" ChString
   , Column "stringNullable" (Nullable ChString)
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
   ]

data TestData = MkTestData
  { dateTime :: DateTime "UTC"
  , dateTimeNullable :: Nullable (DateTime "UTC")
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
  }
  deriving (Generic, Show, Eq)

instance ClickHaskell TestColumns TestData

testData :: TestData
testData = MkTestData
  { dateTime = toChType (0 :: Word32)
  , dateTimeNullable = Just 42
  , int128 = toChType (-128 :: Int128)
  , int128Nullable = toChType $ Just (-128 :: Int128)
  , int16 = toChType (-16 :: Int16)
  , int16Nullable = toChType $ Just (-16 :: Int16)
  , int32 = toChType (-32 :: Int32)
  , int32Nullable = toChType $ Just (-32 :: Int32)
  , int64 = toChType (-64 :: Int64)
  , int64Nullable = toChType $ Just (-64 :: Int64)
  , int8 = toChType (-8 :: Int8)
  , int8Nullable = toChType $ Just (-8 :: Int8)
  , string = "string"
  , stringNullable = Just "string"
  , uint128 = toChType (128 :: Word128)
  , uint128Nullable = toChType $ Just (128 :: Word128)
  , uint16 = toChType (16 :: Word16)
  , uint16Nullable = toChType $ Just (16 :: Word16)
  , uint32 = toChType (32 :: Word32)
  , uint32Nullable = toChType $ Just (32 :: Word32)
  , uint64 = toChType (64 :: Word64)
  , uint64Nullable = toChType $ Just (64 :: Word64)
  , uint8 = toChType (8 :: Word8)
  , uint8Nullable = toChType $ Just (8 :: Word8)
  , uuid = let pos = (^) @Word64 @Word64 16 in
      toChType ((pos 3)*4 + (pos 2)*2  )
    -- ^ 00000000-0000-0000-0000-000000004200
  , uuidNullable = Nothing
  }


{-
<pre><code class="sql" data-lang="sql"
>CREATE TABLE writeReadEqualityTable
(
    `dateTime` DateTime('UTC'),
    `dateTimeNullable` Nullable(DateTime('UTC')),
    `int128` Int128,
    `int128Nullable` Nullable(Int128),
    `int16` Int16,
    `int16Nullable` Nullable(Int16),
    `int32` Int32,
    `int32Nullable` Nullable(Int32),
    `int64` Int64,
    `int64Nullable` Nullable (Int64),
    `int8` Int8,
    `int8Nullable` Nullable(Int8),
    `string` String,
    `stringNullable` Nullable(String),
    `uint128` UInt128,
    `uint128Nullable` Nullable(UInt128),
    `uint16` UInt16,
    `uint16Nullable` Nullable (UInt16),
    `uint32` UInt32,
    `uint32Nullable` Nullable(UInt32),
    `uint64` UInt64,
    `uint64Nullable` Nullable(UInt64),
    `uint8` UInt8,
    `uint8Nullable` Nullable(UInt8),
    `uuid` UUID,
    `uuidNullable` Nullable(UUID)
)
ENGINE = MergeTree
PARTITION BY ()
ORDER BY ();
</code></pre>
-}

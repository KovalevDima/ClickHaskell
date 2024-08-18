{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , LambdaCase
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module ClickHaskell.Writing
  ( WritableInto(..)
  , GWritable(..)
  , Serializable(..)
  ) where


-- Internal dependencies
import ClickHaskell.DbTypes
  ( Nullable
  , LowCardinality, IsLowCardinalitySupported
  , ChInt8, ChInt16, ChInt32, ChInt64, ChInt128
  , ChUInt8, ChUInt16, ChUInt32, ChUInt64, ChUInt128
  , ChString
  , ChUUID
  , ChDateTime
  , ToChType(..)
  , FromChType(..)
  )
import ClickHaskell.Tables (CompiledColumn(..), HasColumns(..), TakeColumn)


-- GHC included
import Data.ByteString         (StrictByteString)
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (concatMap, length, pack, replicate, singleton)
import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.UUID as UUID (toASCIIBytes)
import Data.Word (Word32)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)


-- * Writing

class
  ( HasColumns table
  , GWritable (GetColumns table) (Rep record)
  )
  =>
  WritableInto table record
  where
  default toTsvLine :: (Generic record) => record -> BS.Builder
  toTsvLine :: record -> BS.Builder
  toTsvLine = gToTsvBs @(GetColumns table) . from

  default writingColumns :: Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns table) @(Rep record)


class GWritable
  (columns :: [Type])
  f
  where
  gToTsvBs :: f p -> Builder
  gWritingColumns :: Builder

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  gToTsvBs (M1 (M1 re)) = gToTsvBs @columns re <> "\n"
  gWritingColumns = gWritingColumns @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  gToTsvBs ((left1 :*: left2) :*: right) = gToTsvBs @columns (left1 :*: (left2 :*: right))
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( Serializable (GetColumnType column)
  , ToChType (GetColumnType column) inputType
  , CompiledColumn column
  , GWritable restColumns right
  , GWritable '[column] ((S1 (MetaSel (Just typeName) a b f)) (Rec0 inputType))
  , '(column, restColumns) ~ TakeColumn typeName columns
  )
  =>
  GWritable columns ((S1 (MetaSel (Just typeName) a b f)) (Rec0 inputType) :*: right)
  where
  gToTsvBs (M1 (K1 dataType) :*: right)
    =  (serialize . toChType @(GetColumnType column)) dataType
    <> "\t"
    <> gToTsvBs @restColumns right
  gWritingColumns = renderColumnName @column <> ", " <> gWritingColumns @restColumns @right

instance
  ( ThereIsNoWriteRequiredColumns restColumns
  , Serializable (GetColumnType column)
  , ToChType (GetColumnType column) inputType
  , CompiledColumn column
  , '(column, restColumns) ~ TakeColumn typeName columns
  ) =>
  GWritable columns (S1 (MetaSel (Just typeName) a b f) (Rec0 inputType))
  where
  gToTsvBs = serialize . toChType @(GetColumnType column) @inputType . unK1 . unM1
  gWritingColumns = renderColumnName @column


type family ThereIsNoWriteRequiredColumns (columns :: [Type]) :: Constraint where
  ThereIsNoWriteRequiredColumns '[] = ()
  ThereIsNoWriteRequiredColumns (column ': columns) =
    If
      (WriteOptionalColumn column)
      (ThereIsNoWriteRequiredColumns columns)
      (TypeError ('Text "Column " :<>: 'Text (GetColumnName column) :<>: 'Text " is required for insert but is missing"))




-- * Serialization

class
  Serializable chType
  where
  serialize :: chType -> Builder

instance
  Serializable chType
  =>
  Serializable (Nullable chType)
  where
  serialize = maybe "\\N" serialize

instance
  ( Serializable chType
  , FromChType chType chType
  , IsLowCardinalitySupported chType
  ) =>
  Serializable (LowCardinality chType)
  where
  serialize = serialize @chType . fromChType @(LowCardinality chType)

instance Serializable ChUUID
  where
  serialize = BS.byteString . UUID.toASCIIBytes . fromChType

instance Serializable ChString
  where
  serialize = (BS.byteString . escape) . fromChType

escape :: StrictByteString -> StrictByteString
escape -- [ClickHaskell.DbTypes.ToDo.2]: Optimize
  = BS8.concatMap
    (\case
      '\t' -> "\\t"
      '\n' -> "\\n"
      sym -> BS8.singleton sym
    )

instance Serializable ChInt8
  where
  serialize = BS.byteString . BS8.pack . show @ChInt8

instance Serializable ChInt16
  where
  serialize = BS.byteString . BS8.pack . show @ChInt16 

instance Serializable ChInt32
  where
  serialize = BS.byteString . BS8.pack . show @ChInt32

instance Serializable ChInt64
  where
  serialize = BS.byteString . BS8.pack . show @ChInt64

instance Serializable ChInt128
  where
  serialize = BS.byteString . BS8.pack . show @ChInt128

instance Serializable ChUInt8
  where
  serialize = BS.byteString. BS8.pack . show @ChUInt8

instance Serializable ChUInt16
  where
  serialize = BS.byteString . BS8.pack . show @ChUInt16

instance Serializable ChUInt32
  where
  serialize = BS.byteString . BS8.pack . show . fromChType @ChUInt32 @Word32

instance Serializable ChUInt64
  where
  serialize = BS.byteString . BS8.pack . show @ChUInt64

instance Serializable ChUInt128
  where
  serialize = BS.byteString . BS8.pack . show @ChUInt128

instance Serializable ChDateTime where
  serialize chDateTime
    = let time = BS8.pack . show . fromChType @ChDateTime @Word32 $ chDateTime
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)

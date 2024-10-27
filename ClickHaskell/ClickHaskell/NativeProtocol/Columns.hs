{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , TypeFamilyDependencies
  , UndecidableInstances
  , GADTs
  , ScopedTypeVariables
#-}

module ClickHaskell.NativeProtocol.Columns where

-- Internal
import ClickHaskell.DbTypes (IsChType(..), UVarInt, ChString, ToChType (..), ChUInt8)
import ClickHaskell.NativeProtocol.Serialization


-- GHC included
import Data.ByteString (toStrict)
import Data.ByteString.Builder as BS (Builder, stringUtf8, toLazyByteString)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


-- * Columns

data Columns columns where
  Empty :: Columns '[]
  AddColumn :: Column name chType -> Columns cols -> Columns (Column name chType ': cols)

columnsCount :: Columns columns -> UVarInt
columnsCount Empty = 0
columnsCount (AddColumn _ restColuns) = 1 + columnsCount restColuns

rowsCount :: Columns columns -> UVarInt
rowsCount Empty = 0
rowsCount (AddColumn (MkColumn vec) _) = fromIntegral $ length vec


appendColumn :: Column name chType -> Columns xs -> Columns (Column name chType ': xs)
appendColumn = AddColumn

emptyColumns :: Columns '[]
emptyColumns = Empty



data ColumnWithType = MkColumnWithType
  { column_name :: ChString
  , column_type :: ChString
  }
  deriving (Generic, Serializable)


class ColumnTypesAndNames columns where columnsTypesAndNames :: [ColumnWithType]

instance ColumnTypesAndNames '[] where columnsTypesAndNames = []

instance
  (CompiledColumn col, ColumnTypesAndNames xs)
  =>
  ColumnTypesAndNames (col ': xs)
  where
  columnsTypesAndNames = MkColumnWithType
    (toChType . toStrict . toLazyByteString $ renderColumnName @col)
    (toChType . toStrict . toLazyByteString $ renderColumnType @col)
    : columnsTypesAndNames @xs


-- ** Columns serialization
-- No columns special case
instance Serializable (Columns '[]) where
  serialize _ _ = ""

instance
  ( Serializable (Column name1 chType1)
  , Serializable (Columns (one ': xs))
  )
  =>
  Serializable (Columns (Column name1 chType1 ': one ': xs)) where
  serialize rev (AddColumn column extraColumns) = serialize rev column <> serialize rev extraColumns

instance
  Serializable (Column name chType)
  =>
  Serializable (Columns '[Column name chType])
  where
  serialize rev (AddColumn column Empty) = serialize rev column




-- ** Take column by name from list of columns

type family
  TakeColumn (name :: Symbol) (columns :: [Type]) :: (Type, [Type])
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: (Type, [Type])
  where
  GoTakeColumn name (column ': columns) acc = If (name == GetColumnName column) '(column, acc ++ columns) (GoTakeColumn name columns (column ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )

type family
  (++) (list1 :: [Type]) (list2 :: [Type]) :: [Type]
  where
  (++) '[]            list = list
  (++) (head ': tail) list = tail ++ (head ': list)




-- ** Column declaration

mkColumn :: [chType] -> Column name chType
mkColumn = MkColumn

{- |
Column declaration

Examples:

@
type MyColumn = Column "myColumn" ChString
type MyColumn = Column "myColumn" ChString -> Alias
type MyColumn = Column "myColumn" ChString -> Default
@
-}
data Column (name :: Symbol) (chType :: Type) = MkColumn [chType]

instance
  ( CompiledColumn (Column name chType)
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name chType) where
  serialize rev (MkColumn values)
    =  serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) values)


instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => CompiledColumn (Column name columnType)
  where
  type GetColumnName (Column name columnType) = name
  renderColumnName = (stringUtf8 . symbolVal @name) Proxy

  type GetColumnType (Column name columnType) = columnType
  renderColumnType = chTypeName @columnType

  type WritableColumn (Column _ _) = Nothing

  type WriteOptionalColumn (Column name columnType) = IsWriteOptional columnType




-- ** Columns properties

{- |
Column that refers to another column.

Can be only readed.

Example:

@
type MyColumn = Column "myColumn" ChString -> Alias
@
-}
data Alias

instance
  CompiledColumn (Column name columnType)
  =>
  CompiledColumn (Column name columnType -> Alias)
  where
  type GetColumnName (Column name columnType -> Alias) = GetColumnName (Column name columnType)
  renderColumnName = renderColumnName @(Column name columnType)

  type GetColumnType (Column name columnType -> Alias) = GetColumnType (Column name columnType)
  renderColumnType = renderColumnType @(Column name columnType)

  type WritableColumn (Column name columnType -> Alias) =
    Just
      (    'Text "You are trying insert into Alias column \"" :<>: 'Text name :<>: 'Text "\""
      :$$: 'Text "You can't do this. Read about Alias columns"
      )

  type WriteOptionalColumn (Column name columnType -> Alias) = False


{- |
Column which value could be evaluated when it's not mentioned.

Not required for writing.

Example:

@
type MyColumn = Column "myColumn" ChString -> Default
@
-}
data Default

instance
  CompiledColumn (Column name columnType)
  =>
  CompiledColumn (Column name columnType -> Default)
  where
  type GetColumnName (Column name columnType -> Default) = GetColumnName (Column name columnType)
  renderColumnName = renderColumnName @(Column name columnType)

  type GetColumnType (Column name columnType -> Default) = GetColumnType (Column name columnType)
  renderColumnType = renderColumnType @(Column name columnType)

  type WritableColumn (Column name columnType -> Default) = Nothing

  type WriteOptionalColumn (Column name columnType -> Default) = True




-- ** Compiler

class
  IsChType (GetColumnType columnDescription)
  =>
  CompiledColumn columnDescription where
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Builder

  type GetColumnType columnDescription :: Type
  renderColumnType :: Builder

  type WritableColumn    columnDescription :: Maybe ErrorMessage
  type WriteOptionalColumn columnDescription :: Bool


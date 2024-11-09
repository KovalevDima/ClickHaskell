{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , UndecidableInstances
  , GADTs
  , OverloadedStrings
  , TypeFamilyDependencies
  , InstanceSigs
#-}

module ClickHaskell.NativeProtocol.Columns where

-- Internal
import ClickHaskell.DbTypes (IsChType(..), UVarInt, ChString, ChUInt8, ToChType (toChType))
import ClickHaskell.NativeProtocol.Serialization


-- GHC included
import Data.ByteString (toStrict)
import Data.ByteString.Builder as BS (Builder, stringUtf8, toLazyByteString)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits (ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal, type(+))
import Data.Binary (Get)
import Control.Monad (replicateM)

-- * Columns

data Columns (columns :: [Type]) where
  Empty :: Columns '[]
  AddColumn
    :: KnownColumn (Column name chType)
    => Column name chType
    -> Columns columns
    -> Columns (Column name chType ': columns)

emptyColumns :: Columns '[]
emptyColumns = Empty

appendColumn
  :: KnownColumn (Column name chType)
  => Column name chType
  -> Columns columns
  -> Columns (Column name chType ': columns)
appendColumn = AddColumn

class KnownNat (ColumnsCount columns) => KnownColumns columns
  where
  type ColumnsCount columns :: Nat
  columnsCount :: UVarInt
  columnsCount = (fromIntegral . natVal) (Proxy @(ColumnsCount columns))

  rowsCount :: columns -> UVarInt


instance KnownColumns (Columns '[])
  where
  type ColumnsCount (Columns '[]) = 0
  rowsCount Empty = 0


instance
  KnownNat (1 + ColumnsCount (Columns extraColumns))
  =>
  KnownColumns (Columns (col ': extraColumns))
  where
  type ColumnsCount (Columns (col ': extraColumns)) = 1 + ColumnsCount (Columns extraColumns)
  rowsCount :: Columns (col : extraColumns) -> UVarInt
  rowsCount (AddColumn (MkColumn col) _) = fromIntegral (length col)

-- * Columns extraction helper

class
  HasColumns hasColumns
  where
  type GetColumns hasColumns :: [Type]

instance HasColumns (Columns columns)
  where
  type GetColumns (Columns columns) = columns



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


-- ** 

instance {-# OVERLAPPING #-}
  Serializable (Columns '[])
  where
  serialize _rev Empty = ""

instance {-# OVERLAPPING #-}
  ( Serializable (Columns columns)
  , Serializable (Column name chType)
  )
  =>
  Serializable (Columns (Column name chType ': columns))
  where
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name chType)
  , IsChType chType
  , KnownSymbol name
  , Serializable chType
  ) => Serializable (Column name chType) where
  serialize rev (MkColumn values)
    =  serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString . toStrict . toLazyByteString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) values)




class DeserializableColumns columns where
  deserializeColumns :: ProtocolRevision -> UVarInt -> Get columns

instance
  DeserializableColumns (Columns '[])
  where
  deserializeColumns _rev _rows = pure Empty

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  , DeserializableColumns (Columns extraColumns)
  )
  =>
  DeserializableColumns (Columns (Column name chType ': extraColumns))
  where
  deserializeColumns rev rows = do
    AddColumn
      <$> desializeColumn @name @chType rev rows
      <*> deserializeColumns @(Columns extraColumns) rev rows




desializeColumn :: forall name chType . Deserializable chType => ProtocolRevision -> UVarInt -> Get (Column name chType)
desializeColumn rev rows = do
  _columnName <- deserialize @ChString rev
  _columnType <- deserialize @ChString rev
  _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
  column <- replicateM (fromIntegral rows) (deserialize @chType rev)
  pure $ MkColumn column




-- ** Column declaration

class
  ( IsChType (GetColumnType columnDescription)
  , KnownSymbol (GetColumnName columnDescription)
  , KnownSymbol (ToChTypeName (GetColumnType columnDescription))
  )
  =>
  KnownColumn columnDescription where
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Builder

  type GetColumnType columnDescription :: Type
  renderColumnType :: Builder

  type WritableColumn    columnDescription :: Maybe ErrorMessage
  type WriteOptionalColumn columnDescription :: Bool

{- |
Column declaration

Examples:

@
type MyColumn = Column "myColumn" ChString
type MyColumn = Column "myColumn" ChString -> Alias
type MyColumn = Column "myColumn" ChString -> Default
@
-}
newtype Column (name :: Symbol) (chType :: Type) = MkColumn [chType]

mkColumn :: forall name chType . [chType] -> Column name chType
mkColumn = MkColumn


instance
  ( IsChType columnType
  , KnownSymbol (ToChTypeName columnType)
  , KnownSymbol name
  ) => KnownColumn (Column name columnType)
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
  KnownColumn (Column name columnType)
  =>
  KnownColumn (Column name columnType -> Alias)
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
  KnownColumn (Column name columnType)
  =>
  KnownColumn (Column name columnType -> Default)
  where
  type GetColumnName (Column name columnType -> Default) = GetColumnName (Column name columnType)
  renderColumnName = renderColumnName @(Column name columnType)

  type GetColumnType (Column name columnType -> Default) = GetColumnType (Column name columnType)
  renderColumnType = renderColumnType @(Column name columnType)

  type WritableColumn (Column name columnType -> Default) = Nothing

  type WriteOptionalColumn (Column name columnType -> Default) = True

{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , TypeFamilyDependencies
#-}

module ClickHaskell.Columns where

-- Internal
import ClickHaskell.DbTypes
import ClickHaskell.NativeProtocol

-- GHC included
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal, ErrorMessage(..), KnownSymbol(..), Symbol, TypeError, symbolVal, type (+))
import Data.ByteString.Builder as BS (Builder, stringUtf8)
import Data.Kind (Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
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

{-# INLINE [0] appendColumn #-}
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
  rowsCount (AddColumn (MkColumn size _col) _) = size

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
  GoTakeColumn name (column ': columns) acc =
    If (name == GetColumnName column) '(column, acc ++ columns) (GoTakeColumn name columns (column ': acc))
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

instance
  Serializable (Columns '[])
  where
  {-# INLINE serialize #-}
  serialize _rev Empty = ""

instance
  ( Serializable (Columns columns)
  , Serializable (Column name chType)
  )
  =>
  Serializable (Columns (Column name chType ': columns))
  where
  {-# INLINE serialize #-}
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance
  ( KnownColumn (Column name chType)
  , IsChType chType
  , KnownSymbol name
  , Serializable chType
  ) => Serializable (Column name chType) where
  {-# INLINE serialize #-}
  serialize rev (MkColumn _size values)
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) values)




class DeserializableColumns columns where
  deserializeColumns :: ProtocolRevision -> UVarInt -> Get columns

instance
  DeserializableColumns (Columns '[])
  where
  {-# INLINE deserializeColumns #-}
  deserializeColumns _rev _rows = pure Empty

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  , DeserializableColumns (Columns extraColumns)
  )
  =>
  DeserializableColumns (Columns (Column name chType ': extraColumns))
  where
  {-# INLINE deserializeColumns #-}
  deserializeColumns rev rows = do
    AddColumn
      <$> (do
        _columnName <- deserialize @ChString rev
        _columnType <- deserialize @ChString rev
        _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
        column <- replicateM (fromIntegral rows) (deserialize @chType rev)
        pure $ MkColumn rows column
      )
      <*> deserializeColumns @(Columns extraColumns) rev rows


-- ** Column declaration

{- |
Column declaration

For example:

@
type MyColumn = Column "myColumn" ChString
@
-}
data Column (name :: Symbol) (chType :: Type) = MkColumn UVarInt [chType]

mkColumn :: forall name chType . UVarInt -> [chType] -> Column name chType
mkColumn = MkColumn

class
  ( IsChType (GetColumnType columnDescription)
  , KnownSymbol (GetColumnName columnDescription)
  , KnownSymbol (ToChTypeName (GetColumnType columnDescription))
  )
  =>
  KnownColumn columnDescription where
  type GetColumnName columnDescription :: Symbol
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName columnDescription)) Proxy

  type GetColumnType columnDescription :: Type
  renderColumnType :: Builder
  renderColumnType = chTypeName @(GetColumnType columnDescription)


instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => KnownColumn (Column name columnType)
  where
  type GetColumnName (Column name columnType) = name
  type GetColumnType (Column name columnType) = columnType

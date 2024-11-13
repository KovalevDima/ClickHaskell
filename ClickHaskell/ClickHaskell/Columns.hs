{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , TypeFamilyDependencies
#-}

module ClickHaskell.Columns where

-- Internal dependencies
import ClickHaskell.DbTypes

-- GHC included
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal, ErrorMessage(..), Symbol, TypeError, type (+))
import Data.Kind (Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))

-- * Columns

mkColumn :: forall name chType . UVarInt -> [chType] -> Column name chType
mkColumn = MkColumn

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

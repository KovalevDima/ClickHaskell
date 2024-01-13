{-# LANGUAGE
    DataKinds
  , UndecidableInstances
#-}
module ClickHaskell.Columns.Alias
( AliasColumn
) where


-- Internal
import ClickHouse.DbTypes (IsChType(ToChTypeName))
import ClickHaskell.Columns.Interpreter (InterpretedColumn(..))


-- GHC included
import Data.Kind (Type)
import Data.Text as T (pack)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal, ErrorMessage(..))
import Data.Data (Proxy(..))




{- |
Column that refers to another column.

@
-- Can be only readed:
type IsWritableColumn (AliasColumn _ _) =
  Just
    (    'Text "You are trying insert Alias column \\"" :<>: 'Text name :<>: 'Text "\\""
    :$$: 'Text "You can't do this. Read about Alias columns"
    )
@
-}
data AliasColumn (name :: Symbol) (columnType :: Type)

instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => InterpretedColumn (AliasColumn name columnType)
  where
  type GetColumnName (AliasColumn name _) = name
  renderColumnName = T.pack . symbolVal $ Proxy @name

  type GetColumnType (AliasColumn _ columnType) = columnType
  renderColumnType = T.pack . symbolVal $ Proxy @(ToChTypeName columnType)

  type WritableColumn (AliasColumn name _) =
    Just
      (    'Text "You are trying insert Alias column \"" :<>: 'Text name :<>: 'Text "\""
      :$$: 'Text "You can't do this. Read about Alias columns"
      )

  type WriteOptionalColumn (AliasColumn name _) = Nothing

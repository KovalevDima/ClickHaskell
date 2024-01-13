{-# LANGUAGE
    DataKinds
  , UndecidableInstances
#-}
module ClickHaskell.Columns.Default
( DefaultableColumn
) where


-- Internal
import ClickHouse.DbTypes (IsChType(ToChTypeName))
import ClickHaskell.Columns.Interpreter (InterpretedColumn(..))


-- GHC included
import Data.Data      (Proxy(..))
import Data.Kind      (Type)
import Data.Text as T (pack)
import GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)


{- |
Column which value could be evaluated when it's not mentioned.

@
-- Not required for writing:
type WriteOptionalColumn (DefaultableColumn _ _) = Nothing
@
-}
data DefaultableColumn (name :: Symbol) (columnType :: Type)

instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => InterpretedColumn (DefaultableColumn name columnType)
  where
  type GetColumnName (DefaultableColumn name _) = name
  renderColumnName = T.pack . symbolVal $ Proxy @name

  type GetColumnType (DefaultableColumn _ columnType) = columnType
  renderColumnType = T.pack . symbolVal $ Proxy @(ToChTypeName columnType)

  type WritableColumn (DefaultableColumn name _) = Nothing

  type WriteOptionalColumn (DefaultableColumn name _) = Nothing

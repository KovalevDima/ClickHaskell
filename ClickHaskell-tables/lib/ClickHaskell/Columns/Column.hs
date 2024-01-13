{-# LANGUAGE
    DataKinds
  , UndecidableInstances
#-}
module ClickHaskell.Columns.Column
( Column
) where


-- Internal
import ClickHouse.DbTypes (IsChType(ToChTypeName, IsWriteOptional))
import ClickHaskell.Columns.Interpreter (InterpretedColumn(..))


-- GHC included
import Data.Data      (Proxy(..))
import Data.Kind      (Type)
import Data.Text as T (pack)
import Data.Type.Bool (If)
import GHC.TypeLits   (KnownSymbol, Symbol, symbolVal, ErrorMessage(..))


{- |
Regular Column without any properties
-}
data Column (name :: Symbol) (columnType :: Type)

instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => InterpretedColumn (Column name columnType)
  where
  type GetColumnName (Column name columnType) = name
  renderColumnName = T.pack . symbolVal $ Proxy @name

  type GetColumnType (Column name columnType) = columnType
  renderColumnType = T.pack . symbolVal $ Proxy @(ToChTypeName columnType)

  type WritableColumn (Column _ _) = Nothing

  type WriteOptionalColumn (Column name columnType) =
    If (IsWriteOptional columnType)
      Nothing
      (Just
        (    'Text "Column with name "
          :<>: 'Text name
          :<>: 'Text " is required for insert."
        :$$: 'Text "Add it to your insertable type"
        )
      )

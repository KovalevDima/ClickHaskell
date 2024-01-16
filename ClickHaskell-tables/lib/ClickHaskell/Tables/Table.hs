{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , NamedFieldPuns
#-}

module ClickHaskell.Tables.Table
( Table, renderTable
) where


-- Internal
import ClickHaskell.Tables.Interpreter
  ( InterpretableTable(..)
  , CompiledColumns(GetColumnsRep,ColumnsCompilationResult)
  )


-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type)
import GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)


-- * Table

newtype Table
  (name :: Symbol)
  (columns :: [Type])
  = MkTable
  { renderedTableName :: Builder
  }


instance
  ( KnownSymbol name
  , CompiledColumns columns
  ) => InterpretableTable (Table name columns)
  where
  type GetTableName    (Table name _)    = name
  type GetTableColumns (Table _ columns) = GetColumnsRep columns
  type ValidatedTable   (Table _ columns) = ColumnsCompilationResult columns
  
  type TableInterpreter (Table name columns) = Table name columns
  interpretTable = MkTable{renderedTableName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""}



{- |
Takes evaluated Table and renders it

>>> renderTable MkTable{renderedTableName="myTableName"}
"myTableName"

>>> renderTable MkTable{renderedTableName="\"space seperated name\""}
"\"space seperated name\""
-}
renderTable :: Table name columns -> Builder
renderTable (MkTable{renderedTableName}) = renderedTableName

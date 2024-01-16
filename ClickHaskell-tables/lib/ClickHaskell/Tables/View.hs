{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , OverloadedStrings
  , NamedFieldPuns
#-}

module ClickHaskell.Tables.View
( View, renderView

, Parameter, mkParameter
) where


-- Internal
import ClickHaskell.Tables.Interpreter
  ( InterpretableTable
    ( GetTableName
    , GetTableColumns
    , ValidatedTable
    , TableInterpreter
    , interpretTable
    )
  , CompiledColumns
    ( ColumnsCompilationResult
    , GetColumnsRep
    )
  )
import ClickHouse.DbTypes (ToQueryPart(toQueryPart))


-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type)
import GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)


-- * View

data View
  (name :: Symbol)
  (columns :: [Type])
  (parameters :: [Type])
  = MkView
    { viewName :: Builder
    , inpterpretedParameters :: [Builder]
    }




newtype Parameter (name :: Symbol) (chType :: Type) =
  MkParameter
    { renderedParameter :: Builder
    }


mkParameter ::
  forall name chType
  .
  ( KnownSymbol name
  , ToQueryPart chType
  ) =>
  chType -> Parameter name chType
mkParameter chType =
  MkParameter
    { renderedParameter = (BS.byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart chType
    }


{- |
Takes evaluated View and renders it

>>> let {param1="param1=['1']"; param2="param2=['2']"}

>>> renderView MkView{viewName="viewExample", inpterpretedParameters=[]}
"viewExample"

>>> renderView MkView{viewName="viewExample2", inpterpretedParameters=[param1]}
"viewExample2(param1=['1'])"

>>> renderView MkView{viewName="viewExample3", inpterpretedParameters=[param1, param2]}
"viewExample3(param2=['2'], param1=['1'])"
-}
renderView :: View name columns parameters -> Builder
renderView (MkView{viewName, inpterpretedParameters}) = viewName <> renderTableParameters inpterpretedParameters


renderTableParameters :: [Builder] -> Builder
renderTableParameters (parameter:ps) = "(" <> foldr (\p1 p2 -> p1 <> ", " <> p2) parameter ps <> ")"
renderTableParameters []             = ""


instance
  ( KnownSymbol name
  , CompiledColumns columns
  ) => InterpretableTable (View name columns '[])
  where
  type GetTableName    (View name _ _)    = name
  type GetTableColumns (View _ columns _) = GetColumnsRep columns
  type ValidatedTable  (View _ columns _) = ColumnsCompilationResult columns

  type TableInterpreter (View name columns '[]) = View name columns '[]
  interpretTable =
    MkView
      { viewName = "\"" <> (BS.byteString . BS8.pack . symbolVal @name) Proxy <> "\""
      , inpterpretedParameters = []
      }


instance
  ( KnownSymbol name
  , CompiledColumns columns
  ) => InterpretableTable (View name columns '[Parameter paramName (chType :: Type)])
  where
  type GetTableName    (View name _ _)    = name
  type GetTableColumns (View _ columns _) = GetColumnsRep columns
  type ValidatedTable  (View _ columns _) = ColumnsCompilationResult columns

  type TableInterpreter (View name columns '[Parameter paramName chType]) = Parameter paramName chType -> View name columns '[]
  interpretTable MkParameter{renderedParameter} =
    MkView
      { viewName = "\"" <> (BS.byteString . BS8.pack . symbolVal  @name) Proxy <> "\""
      , inpterpretedParameters = [renderedParameter]
      }

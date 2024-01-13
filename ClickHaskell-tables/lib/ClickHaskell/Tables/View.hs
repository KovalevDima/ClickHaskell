{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , NamedFieldPuns
#-}

module ClickHaskell.Tables.View
( View, renderView

, Parameter, mkParameter
) where


-- Internal
import ClickHaskell.Tables.Interpreter
  ( TableInterpretable
    ( GetTableName
    , GetTableColumns
    , ValidatedTable
    , TableInterpreter
    , interpretTable
    )
  , IsValidColumns
    ( ColumnsValdationResult
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
  ( ToQueryPart chType
  ) =>
  chType -> Parameter name chType
mkParameter chType =
  MkParameter
    { renderedParameter = toQueryPart chType
    }


{- |
Takes evaluated View and renders it

>>> let {param1="param1=['1']"; param2="param2=['2']"}

>>> renderView MkView{viewName="viewExample", renderedParameters=[]}
"viewExample"

>>> renderView MkView{viewName="viewExample2", renderedParameters=[param1]}
"viewExample2(param1=['1'])"

>>> renderView MkView{viewName="viewExample3", renderedParameters=[param1, param2]}
"viewExample3(param2=['2'], param1=['1'])"
-}
renderView :: View name columns parameters -> Builder
renderView (MkView{viewName, inpterpretedParameters}) = viewName <> renderTableParameters inpterpretedParameters


renderTableParameters :: [Builder] -> Builder
renderTableParameters parameters =
  if null parameters
  then ""
  else "(" <> foldr ((<>) . (<> ", " )) (head parameters) (tail parameters) <> ")"


instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => TableInterpretable (View name columns '[])
  where
  type GetTableName    (View name _ _)    = name
  type GetTableColumns (View _ columns _) = GetColumnsRep columns
  type ValidatedTable   (View _ columns _) = ColumnsValdationResult columns

  type TableInterpreter (View name columns '[]) = View name columns '[]
  interpretTable =
    MkView
      { viewName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""
      , inpterpretedParameters = []
      }


instance
  ( KnownSymbol name
  , IsValidColumns columns
  ) => TableInterpretable (View name columns '[Parameter name (chType :: Type)])
  where
  type GetTableName    (View name _ _)    = name
  type GetTableColumns (View _ columns _) = GetColumnsRep columns
  type ValidatedTable   (View _ columns _) = ColumnsValdationResult columns

  type TableInterpreter (View name columns '[Parameter name chType]) = Parameter name chType -> View name columns '[]
  interpretTable MkParameter{renderedParameter} =
    MkView
      { viewName = "\"" <> (BS.byteString . BS8.pack . symbolVal) (Proxy @name) <> "\""
      , inpterpretedParameters = [renderedParameter]
      }

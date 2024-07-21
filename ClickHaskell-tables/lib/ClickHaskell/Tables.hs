{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , TypeFamilyDependencies
  , UndecidableInstances
  , GADTs
  , ScopedTypeVariables
#-}

module ClickHaskell.Tables
(
-- * Specs
  Table
, View

, parameter
, Parameter

, parameters
, ParametersInterpreter(..)

, InterpretableParameters(..)


-- * Columns
-- ** HasColumns helper class
, Columns
, HasColumns(..)

-- ** Column
, Column

-- ** Column subtypes
, Alias
, Default

-- ** Compilers
, CompiledColumn(..)
) where


-- Internal
import ClickHaskell.DbTypes (ToQueryPart(..), IsChType(ToChTypeName, IsWriteOptional), ToChType, toChType)


-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString, stringUtf8)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type)
import GHC.TypeLits            (ErrorMessage (..), Symbol, KnownSymbol, symbolVal)


-- * Specs

data Table
  (name :: Symbol)
  (columns :: [Type])

data View
  (name :: Symbol)
  (columns :: [Type])
  (parameters :: [Type])

data Parameter (name :: Symbol) (chType :: Type)


-- >>> parameters (parameter @"a3" @ChString ("a3Val" :: ByteString) . parameter @"a2" @ChString ("a2Val" :: ByteString))
-- "(a2='a2Val', a3='a3Val')"
parameters :: forall params . (ParametersInterpreter '[] -> ParametersInterpreter params) -> Builder
parameters interpreter = renderParameters $ interpreter (MkParametersInterpreter [])

parameter
  :: forall name chType parameters userType
  . (InterpretableParameters parameters, ToChType chType userType, KnownSymbol name, ToQueryPart chType)
  => userType -> ParametersInterpreter parameters -> WithPassedParameter (Parameter name chType) parameters
parameter = interpretParameter

renderParameters :: ParametersInterpreter parameters -> Builder
renderParameters (MkParametersInterpreter (param:ps)) = "(" <> foldr (\p1 p2 -> p1 <> ", " <> p2) param ps <> ")"
renderParameters (MkParametersInterpreter [])         = ""




newtype ParametersInterpreter (parameters :: [Type]) =
  MkParametersInterpreter
    { evaluatedParameters :: [Builder]
    }

class InterpretableParameters (ps :: [Type]) where
  type WithPassedParameter p ps = withPassedParameter | withPassedParameter -> ps p
  interpretParameter
    :: forall name chType userType
    . (ToChType chType userType, KnownSymbol name, ToQueryPart chType)
    => userType -> (ParametersInterpreter ps -> WithPassedParameter (Parameter name chType) ps)

instance InterpretableParameters '[]
  where
  type WithPassedParameter p '[] = ParametersInterpreter '[p]
  interpretParameter
    :: forall name userType chType
    . (ToChType chType userType, KnownSymbol name, ToQueryPart chType)
    => userType -> ParametersInterpreter '[] -> WithPassedParameter (Parameter name chType) '[]
  interpretParameter userType _ = MkParametersInterpreter [renderParameter @name @chType userType]

instance InterpretableParameters (x ': xs)
  where
  type WithPassedParameter p (x ': xs) = ParametersInterpreter (p ': (x ': xs))
  interpretParameter
    :: forall name userType chType
    . (ToChType chType userType, KnownSymbol name, ToQueryPart chType)
    => userType -> ParametersInterpreter (x : xs) -> WithPassedParameter (Parameter name chType) (x : xs)
  interpretParameter userType (MkParametersInterpreter evaluatedParameters) =
    MkParametersInterpreter $ renderParameter @name @chType userType : evaluatedParameters 

renderParameter ::
  forall name chType userType
  .
  ( KnownSymbol name
  , ToChType chType userType
  , ToQueryPart chType
  )
  =>
  userType -> Builder
renderParameter chType = (BS.byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart (toChType @chType chType)








-- * Columns

-- ** HasColumns helper class

data Columns (columns :: [Type])

class HasColumns hasColumns where
  type GetColumns hasColumns :: [Type]

instance HasColumns (View name columns params) where
  type GetColumns (View _ columns _) = columns

instance HasColumns (Table name columns) where
  type GetColumns (Table _ columns) = columns

instance HasColumns (Columns columns) where
  type GetColumns (Columns columns) = columns




-- ** Column declaration

{- |
Column declaration

Examples:

@
type MyColumn = Column "myColumn" ChString
type MyColumn = Column "myColumn" ChString -> Alias
type MyColumn = Column "myColumn" ChString -> Default
@
-}
data Column (name :: Symbol) (columnType :: Type)

instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => CompiledColumn (Column name columnType)
  where
  type GetColumnName (Column name columnType) = name
  renderColumnName = (stringUtf8 . symbolVal @name) Proxy

  type GetColumnType (Column name columnType) = columnType
  renderColumnType = (stringUtf8 . symbolVal @(ToChTypeName columnType)) Proxy

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

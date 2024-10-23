{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
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
, CheckParameters

-- * Columns
-- ** HasColumns helper class
, HasColumns(..)

-- ** Take column from list of columns
, TakeColumn

-- ** Column
, mkColumn
, Column(..)

-- ** Column subtypes
, Alias
, Default

-- ** Compilers
, CompiledColumn(..)
) where


-- Internal
import ClickHaskell.DbTypes (ToQueryPart(..), IsChType(ToChTypeName, IsWriteOptional), ToChType, toChType, chTypeName)


-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString, stringUtf8)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type, Constraint)
import GHC.TypeLits            (TypeError, ErrorMessage (..), Symbol, KnownSymbol, symbolVal)
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))

-- * Specs

data Table
  (name :: Symbol)
  (columns :: [Type])

data View
  (name :: Symbol)
  (columns :: [Type])
  (parameters :: [Type])

data Parameter (name :: Symbol) (chType :: Type)


-- |
-- >>> parameters (parameter @"a3" @ChString ("a3Val" :: ByteString) . parameter @"a2" @ChString ("a2Val" :: ByteString))
-- "(a2='a2Val', a3='a3Val')"
parameters :: forall (params :: [Type]) . (ParametersInterpreter '[] -> ParametersInterpreter params) -> Builder
parameters interpreter = renderParameters $ interpreter (MkParametersInterpreter [])

parameter
  :: forall name chType parameters userType
  . ( InterpretableParameters parameters, ToChType chType userType, KnownSymbol name, ToQueryPart chType)
  => userType -> ParametersInterpreter parameters -> WithPassedParameter (Parameter name chType) parameters
parameter = interpretParameter . toChType

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
    :: forall name chType
    . (KnownSymbol name, ToQueryPart chType)
    => chType -> (ParametersInterpreter ps -> WithPassedParameter (Parameter name chType) ps)

instance InterpretableParameters '[]
  where
  type WithPassedParameter p '[] = ParametersInterpreter '[p]
  interpretParameter
    :: forall name chType
    . (KnownSymbol name, ToQueryPart chType)
    => chType -> ParametersInterpreter '[] -> WithPassedParameter (Parameter name chType) '[]
  interpretParameter userType _ = MkParametersInterpreter [renderParameter @name @chType userType]

instance InterpretableParameters (x ': xs)
  where
  type WithPassedParameter p (x ': xs) = ParametersInterpreter (p ': (x ': xs))
  interpretParameter
    :: forall name chType
    . (KnownSymbol name, ToQueryPart chType)
    => chType -> ParametersInterpreter (x : xs) -> WithPassedParameter (Parameter name chType) (x : xs)
  interpretParameter chType (MkParametersInterpreter evaluatedParameters) =
    MkParametersInterpreter $ renderParameter @name @chType chType : evaluatedParameters

renderParameter ::
  forall name chType
  .
  ( KnownSymbol name
  , ToQueryPart chType
  )
  =>
  chType -> Builder
renderParameter chType = (BS.byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart chType

class GetParameterInfo p where
  type GetParameterName p :: Symbol
  type GetParameterType p :: Type

instance GetParameterInfo (Parameter name chType) where
  type GetParameterName (Parameter name chType) = name
  type GetParameterType (Parameter name chType) = chType

type family CheckParameters
  (tableFunctionParams :: [Type])
  (passedParams :: [Type])
  :: Constraint
  where
  CheckParameters tfs ps = (CheckDuplicates ps, GoCheckParameters tfs ps '[] True)

type family CheckDuplicates
  (passedParams :: [Type])
  :: Constraint
  where
  CheckDuplicates '[] = ()
  CheckDuplicates (p ': ps) = (CheckParamDuplicates p ps, CheckDuplicates ps)

type family CheckParamDuplicates
  (param :: Type)
  (passedParams :: [Type])
  :: Constraint
  where
  CheckParamDuplicates _ '[] = ()
  CheckParamDuplicates p' (p ': ps) = If
    (GetParameterName p' == GetParameterName p)
    (TypeError ('Text "Duplicated parameter \"" :<>: 'Text (GetParameterName p) :<>: 'Text "\" in passed parameters"))
    (CheckParamDuplicates p' ps)

type family GoCheckParameters
  (tableFunctionParams :: [Type])
  (passedParams :: [Type])
  (acc :: [Type])
  (firstRound :: Bool)
  :: Constraint
  where
  GoCheckParameters '[] '[] '[] _ = ()
  GoCheckParameters (p ': _) '[] '[] _ = TypeError
    ('Text "Missing  \"" :<>: 'Text (GetParameterName p) :<>: 'Text "\" in passed parameters.")
  GoCheckParameters '[] (p ': _) _ _ = TypeError
    ('Text "More parameters passed than used in the view")
  GoCheckParameters '[] '[] (p ': _) _ = TypeError
    ('Text "More parameters passed than used in the view")
  GoCheckParameters (p ': ps) '[] (p' ': ps') False = TypeError
    ('Text "Missing  \"" :<>: 'Text (GetParameterName p) :<>: 'Text "\" in passed parameters")
  GoCheckParameters (p ': ps) '[] (p' ': ps') True = GoCheckParameters (p ': ps) (p' ': ps') '[] False
  GoCheckParameters (p ': ps) (p' ': ps') acc b = If
    (GetParameterName p == GetParameterName p')
    (GoCheckParameters ps ps' acc True)
    (GoCheckParameters (p ': ps) ps' (p' ': acc) b)






-- * Columns

-- ** HasColumns helper class

class HasColumns (hasColumns :: k) where
  type GetColumns hasColumns :: [Type]

instance HasColumns (View name columns params) where
  type GetColumns (View _ columns _) = columns

instance HasColumns (Table name columns) where
  type GetColumns (Table _ columns) = columns

instance HasColumns (columns :: [Type]) where
  type GetColumns columns = columns




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




-- ** Column declaration

mkColumn :: [chType] -> Column name chType
mkColumn = MkColumn

{- |
Column declaration

Examples:

@
type MyColumn = Column "myColumn" ChString
type MyColumn = Column "myColumn" ChString -> Alias
type MyColumn = Column "myColumn" ChString -> Default
@
-}
data Column (name :: Symbol) (chType :: Type) = MkColumn [chType]


instance
  ( IsChType columnType
  , KnownSymbol name
  , KnownSymbol (ToChTypeName columnType)
  ) => CompiledColumn (Column name columnType)
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


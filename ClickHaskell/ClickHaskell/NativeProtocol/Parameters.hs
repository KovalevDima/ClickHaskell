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

module ClickHaskell.NativeProtocol.Parameters where

-- Internal
import ClickHaskell.DbTypes (ToQueryPart(..), ToChType(..))


-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type, Constraint)
import GHC.TypeLits            (TypeError, ErrorMessage (..), Symbol, KnownSymbol, symbolVal)
import Data.Type.Bool          (If)
import Data.Type.Equality      (type(==))


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
  CheckParamDuplicates (Parameter name1 chType) (Parameter name2 _ ': ps) = If
    (name1 == name2)
    (TypeError ('Text "Duplicated parameter \"" :<>: 'Text name1 :<>: 'Text "\" in passed parameters"))
    (CheckParamDuplicates (Parameter name1 chType) ps)

type family GoCheckParameters
  (tableFunctionParams :: [Type])
  (passedParams :: [Type])
  (acc :: [Type])
  (firstRound :: Bool)
  :: Constraint
  where
  GoCheckParameters '[] '[] '[] _ = ()
  GoCheckParameters (Parameter name _ ': _) '[] '[] _ = TypeError
    ('Text "Missing  \"" :<>: 'Text name :<>: 'Text "\" in passed parameters.")
  GoCheckParameters '[] (p ': _) _ _ =
    TypeError ('Text "More parameters passed than used in the view")
  GoCheckParameters '[] '[] (p ': _) _ =
    TypeError ('Text "More parameters passed than used in the view")
  GoCheckParameters (Parameter name1 _ ': ps) '[] (Parameter name2 _ ': ps') False =
    TypeError ('Text "Missing  \"" :<>: 'Text name1 :<>: 'Text "\" in passed parameters")
  GoCheckParameters (p ': ps) '[] (p' ': ps') True =
    GoCheckParameters (p ': ps) (p' ': ps') '[] False
  GoCheckParameters (Parameter name1 _ ': ps) (Parameter name1 _ ': ps') acc b =
    (GoCheckParameters ps ps' acc True)
  GoCheckParameters (Parameter name1 chType1 ': ps) (Parameter name2 chType2 ': ps') acc b =
    (GoCheckParameters (Parameter name1 chType1 ': ps) ps' (Parameter name2 chType2 ': acc) b)

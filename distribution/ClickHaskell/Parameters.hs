{-# LANGUAGE
    OverloadedStrings
  , TypeFamilyDependencies
  , UndecidableInstances
#-}

module ClickHaskell.Parameters where

-- Internal
import ClickHaskell.DbTypes (ToQueryPart(..), ToChType(..))

-- GHC included
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (pack)
import Data.Data               (Proxy (Proxy))
import Data.Kind               (Type, Constraint)
import GHC.TypeLits            (TypeError, ErrorMessage (..), Symbol, KnownSymbol, symbolVal)

type family KnownParameter param
  where
  KnownParameter (Parameter name parType) = (KnownSymbol name, ToQueryPart parType)

data Parameter (name :: Symbol) (chType :: Type) = MkParamater chType

data Parameters parameters where
  NoParameters :: Parameters '[]
  AddParameter
    :: KnownParameter (Parameter name chType)
    => Parameter name chType
    -> Parameters parameters
    -> Parameters (Parameter name chType ': parameters)

-- >>> import ClickHaskell.DbTypes
{- |
>>> parameters (parameter @"a3" @ChString ("a3Val" :: String) . parameter @"a2" @ChString ("a2Val" :: String))
"(a3='a3Val', a2='a2Val')"
-}
parameters :: (Parameters '[] -> Parameters passedParameters) -> Builder
parameters interpreter = "(" <> renderParameters (interpreter NoParameters) <> ")"

renderParameters :: Parameters params -> Builder
renderParameters NoParameters                      = ""
renderParameters (AddParameter param NoParameters) = renderParameter param
renderParameters (AddParameter param moreParams)   = renderParameter param <> ", " <> renderParameters moreParams


parameter
  :: forall name chType parameters userType
  . (ToChType chType userType, KnownParameter (Parameter name chType))
  => userType -> Parameters parameters -> Parameters (Parameter name chType ': parameters)
parameter val = AddParameter (MkParamater $ toChType val)

renderParameter :: forall name chType . KnownParameter (Parameter name chType) => Parameter name chType -> Builder
renderParameter (MkParamater chType) = (BS.byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart chType

type family CheckParameters (required :: [Type]) (passed :: [Type]) :: Constraint
  where
  CheckParameters required passed = GoCheckParameters required passed '[] 

type family GoCheckParameters required passed acc :: Constraint
  where
  GoCheckParameters '[] '[] '[] = ()
  GoCheckParameters (Parameter name _ ': _) '[] '[] = TypeError ('Text "Missing parameter \"" :<>: 'Text name :<>: 'Text "\".")
  GoCheckParameters '[] (p ': _) _ = TypeError ('Text "More parameters passed than used in the view")
  GoCheckParameters '[] '[] (p ': _) = TypeError ('Text "More parameters passed than used in the view")
  GoCheckParameters (Parameter name1 _ ': ps) '[] (Parameter name2 _ ': ps') = TypeError ('Text "Missing  \"" :<>: 'Text name1 :<>: 'Text "\" in passed parameters")
  GoCheckParameters (p ': ps) '[] (p' ': ps') = GoCheckParameters (p ': ps) (p' ': ps') '[]
  GoCheckParameters (Parameter name1 _ ': ps) (Parameter name1 _ ': ps') acc = (GoCheckParameters ps ps' acc)
  GoCheckParameters (Parameter name1 chType1 ': ps) (Parameter name2 chType2 ': ps') acc
    = (GoCheckParameters (Parameter name1 chType1 ': ps) ps' (Parameter name2 chType2 ': acc))

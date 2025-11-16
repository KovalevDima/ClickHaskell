module ClickHaskell.Statements where

-- Internal
import ClickHaskell.Primitive
import ClickHaskell.Packets.Settings (DbSettings (..))

-- GHC included
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Kind (Type)
import Data.List (intersperse)
import Data.Proxy (Proxy (..))
import GHC.TypeLits


-- * Statements

tableName :: forall name . KnownSymbol name => Builder
tableName = (byteString . BS8.pack) (symbolVal $ Proxy @name)

class Statement statement where
  {- |
    Wrapper for settings passing
  -}
  passSettings :: (DbSettings -> DbSettings) -> statement -> statement

instance Statement (Select cols output) where
  passSettings pass (MkSelect mkQuery dbSettings) = MkSelect mkQuery (pass dbSettings)

instance Statement (Insert cols input) where
  passSettings pass (MkInsert mkQuery dbSettings) = MkInsert mkQuery (pass dbSettings)

-- ** SELECT

{-|
  SELECT statement abstraction
-}
data Select (columns :: [Type]) output
  where
  MkSelect :: ([(Builder, Builder)] -> ChString) -> DbSettings -> Select columns output

unsafeMkSelect :: ([(Builder, Builder)] -> Builder) -> Select columns output
unsafeMkSelect s = MkSelect (toChType . s) (MkDbSettings [])

{-|
  Type-safe wrapper for statements like

  @SELECT ${columns} FROM ${table}@
-}
fromTable ::
  forall name columns output
  .
  KnownSymbol name
  =>
  Select columns output
fromTable = unsafeMkSelect $ \cols ->
  "SELECT " <> selectedColumns cols <>
  " FROM " <> tableName @name
  where
  selectedColumns =
    mconcat . intersperse ", " . map (\(name, _) -> name)

fromView ::
  forall name columns output params
  .
  KnownSymbol name
  =>
  (Parameters '[] -> Parameters params) -> Select columns output
fromView interpreter = unsafeMkSelect $ \cols ->
  "SELECT " <> selectedColumns cols <>
  " FROM " <> tableName @name <> viewParameters interpreter
  where
  selectedColumns =
    mconcat . intersperse ", " . map (\(name, _) -> name)

fromGenerateRandom ::
  forall columns output
  .
  (UInt64, UInt64, UInt64) -> UInt64 -> Select columns output
fromGenerateRandom (randomSeed, maxStrLen, maxArrayLen) limit = query
  where
  query = unsafeMkSelect $ \cols ->
    "SELECT * FROM generateRandom(" <>
        "'" <> columnsAndTypes cols <> "'" <> "," <>
        toQueryPart randomSeed <> "," <>
        toQueryPart maxStrLen <> "," <>
        toQueryPart maxArrayLen <>
      ")" <>
    " LIMIT " <> toQueryPart limit <> ";"

  columnsAndTypes =
    mconcat . intersperse ", " . map (\(name, tyype) -> name <> " " <> tyype)


-- ** INSERT

{-|
  INSERT statement generation abstraction
-}
data Insert (columns :: [Type]) output
  where
  MkInsert :: ([(Builder, Builder)] -> ChString) -> DbSettings -> Insert columns output

unsafeMkInsert :: ([(Builder, Builder)] -> Builder) -> Insert columns output
unsafeMkInsert s = MkInsert (toChType . s) (MkDbSettings [])

intoTable ::
  forall name columns output
  .
  KnownSymbol name
  =>
  Insert columns output
intoTable = unsafeMkInsert mkQuery
  where
  mkQuery cols =
    "INSERT INTO " <> tableName @name <>
    " (" <> mkInsertColumns cols <> ") VALUES"
  mkInsertColumns cols =
    (mconcat . intersperse ", " . map (\(name, _) -> name)) cols




-- * Parameters

type family KnownParameter param
  where
  KnownParameter (Parameter name parType) = (KnownSymbol name, IsChType parType, ToQueryPart parType)

data Parameter (name :: Symbol) (chType :: Type) = MkParamater chType

data Parameters parameters where
  NoParameters :: Parameters '[]
  AddParameter
    :: KnownParameter (Parameter name chType)
    => Parameter name chType
    -> Parameters parameters
    -> Parameters (Parameter name chType ': parameters)

{- |
>>> viewParameters (parameter @"a3" ("a3Val" :: ChString) . parameter @"a2" ("a2Val" :: ChString))
"(a3='a3Val', a2='a2Val')"
-}
viewParameters :: (Parameters '[] -> Parameters passedParameters) -> Builder
viewParameters interpreter = "(" <> renderParameters (interpreter NoParameters) <> ")"

renderParameters :: Parameters params -> Builder
renderParameters NoParameters                      = ""
renderParameters (AddParameter param NoParameters) = renderParameter param
renderParameters (AddParameter param moreParams)   = renderParameter param <> ", " <> renderParameters moreParams


parameter
  :: KnownParameter (Parameter name t)
  => t -> Parameters params -> Parameters (Parameter name t ': params)
parameter val = AddParameter (MkParamater val)

renderParameter :: forall name chType . KnownParameter (Parameter name chType) => Parameter name chType -> Builder
renderParameter (MkParamater chType) = (byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart chType

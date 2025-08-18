module ClickHaskell.Statements where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import Data.Bits (Bits (..))
import Data.ByteString as BS (ByteString)
import Data.ByteString.Builder (Builder, byteString, word16HexFixed)
import Data.ByteString.Char8 as BS8 (concatMap, length, pack, replicate, singleton)
import Data.Coerce (coerce)
import Data.Int
import Data.Kind (Type)
import Data.List (intersperse)
import Data.Proxy (Proxy (..))
import Data.Word
import GHC.List (uncons)
import GHC.TypeLits

-- External
import Data.WideWord (Int128 (..), Word128(..))

-- * Statements

tableName :: forall name . KnownSymbol name => Builder
tableName = (byteString . BS8.pack) (symbolVal $ Proxy @name)


-- ** SELECT

{-|
  SELECT statement abstraction
-}
data Select (columns :: [Type]) output
  where
  MkSelect :: ([(Builder, Builder)] -> ChString) -> Select columns output

unsafeMkSelect :: ([(Builder, Builder)] -> Builder) -> Select columns output
unsafeMkSelect s = MkSelect (toChType . s)

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
  MkInsert :: ([(Builder, Builder)] -> ChString) -> Insert columns output

unsafeMkInsert :: ([(Builder, Builder)] -> Builder) -> Insert columns output
unsafeMkInsert s = MkInsert (toChType . s)

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

    
class ToQueryPart chType where toQueryPart :: chType -> Builder
instance ToQueryPart Int8 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int16 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int32 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int64 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart Int128 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt8 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt16 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt32 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt64 where toQueryPart = byteString . BS8.pack . show
instance ToQueryPart UInt128 where toQueryPart w128 = "'" <> (byteString . BS8.pack . show) w128 <> "'"
instance ToQueryPart UInt256 where toQueryPart w256 = "'" <> (byteString . BS8.pack . show) w256 <> "'"
instance ToQueryPart chType => ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart
instance ToQueryPart chType => ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType
instance ToQueryPart UUID where
  toQueryPart (MkUUID (Word128 hi lo)) = mconcat
    ["'", p 3 hi, p 2 hi, "-", p 1 hi, "-", p 0 hi, "-", p 3 lo, "-", p 2 lo, p 1 lo, p 0 lo, "'"]
    where
    p :: Int -> Word64 -> Builder
    p shiftN word = word16HexFixed $ fromIntegral (word `unsafeShiftR` (shiftN*16))
instance ToQueryPart ChString where
  toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"
    where
    escapeQuery :: BS.ByteString -> Builder
    escapeQuery = byteString . BS8.concatMap (\case '\'' -> "\\\'"; '\\' -> "\\\\"; sym -> BS8.singleton sym;)

-- ToDo: Need to be fixed
-- instance ToQueryPart (DateTime64 precision tz)
--   where
--   toQueryPart chDateTime =
--     let time = BS8.pack . show . fromChType @_ @Word64 $ chDateTime
--     in byteString (BS8.replicate (12 - BS8.length time) '0' <> time)

instance ToQueryPart (DateTime tz)
  where
  toQueryPart chDateTime = let time = BS8.pack . show . coerce @(DateTime tz) @Word32 $ chDateTime
    in byteString (BS8.replicate (10 - BS8.length time) '0' <> time)
instance (IsChType chType, ToQueryPart chType) => ToQueryPart (Array chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldr (\a b -> a <> "," <> b))) . uncons
    . map (toQueryPart @chType)) . coerce @(Array chType) @[chType]

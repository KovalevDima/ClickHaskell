module ClickHaskell.Statements where

-- Internal
import ClickHaskell.Primitive

-- GHC included
import GHC.TypeLits
import Data.Kind (Type)
import Data.ByteString.Builder (Builder, byteString, word16HexFixed)
import Data.Int
import Data.ByteString as BS (ByteString)
import Data.Word
import Data.ByteString.Char8 as BS8 (concatMap, singleton, length, pack, replicate)
import Data.Bits (Bits(..))
import Data.Coerce (coerce)
import GHC.List (uncons)
import Data.Proxy (Proxy(..))

-- External
import Data.WideWord (Int128 (..), Word128(..))

-- * Type wrappers

type family GetTableName table :: Symbol
type instance (GetTableName (Table name columns)) = name
type instance (GetTableName (View name columns params)) = name

type family GetColumns table :: [Type]
type instance (GetColumns (Table name columns)) = columns
type instance GetColumns (View name columns params) = columns

tableName :: forall table . KnownSymbol (GetTableName table) => Builder
tableName = (byteString . BS8.pack) (symbolVal $ Proxy @(GetTableName table))


class IsTable table

-- | Type wrapper for statements generation
data Table (name :: Symbol) (columns :: [Type])
instance IsTable (Table name columns) where


class IsView view

-- | Type wrapper for statements generation
data View (name :: Symbol) (columns :: [Type]) (parameters :: [Type])
instance IsView (View name columns parameters)




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
instance ToQueryPart UInt128 where toQueryPart = byteString . BS8.pack . show
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

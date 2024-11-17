{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , LambdaCase
  , OverloadedStrings
  , TupleSections
#-}

module ClickHaskell.DbTypes
( IsChType(ToChTypeName, chTypeName, defaultValueOfTypeName)
, ToChType(toChType)
, FromChType(fromChType)
, ToQueryPart(toQueryPart)

, ChDateTime(..)
, ChDate(..)

, ChInt8(..), ChInt16(..), ChInt32(..), ChInt64(..), ChInt128(..)
, ChUInt8(..), ChUInt16(..), ChUInt32(..), ChUInt64(..), ChUInt128(..)

, ChString(..)
, ChUUID(..)

, ChArray(..)
, Nullable
, LowCardinality, IsLowCardinalitySupported

, UVarInt(..)
, Columns(..)
, KnownColumn(..), Column(..), columnValues, columnSize
, module Data.WideWord
) where

-- Internal dependencies

-- External
import Data.WideWord (Int128 (..), Word128(..))

-- GHC included
import Control.DeepSeq (NFData)
import Data.Bits (Bits (..))
import Data.ByteString as BS (StrictByteString, toStrict)
import Data.ByteString.Builder as BS (Builder, byteString, toLazyByteString, stringUtf8)
import Data.ByteString.Char8 as BS8 (concatMap, length, pack, replicate, singleton)
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List (uncons)
import Data.String (IsString)
import Data.Text as Text (Text)
import Data.Text.Encoding as Text (encodeUtf8)
import Data.Time (UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..))
import Data.Vector.Primitive.Mutable (Prim)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)


class
  KnownSymbol (ToChTypeName chType) =>
  IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- type ToChTypeName ChString = \"String\"
  -- type ToChTypeName (Nullable ChUInt32) = \"Nullable(UInt32)\"
  -- @
  type ToChTypeName chType :: Symbol

  chTypeName :: Builder
  chTypeName = byteString . BS8.pack . symbolVal @(ToChTypeName chType) $ Proxy

  defaultValueOfTypeName :: chType

class IsChType chType => ToChType chType inputType    where toChType    :: inputType -> chType
class IsChType chType => FromChType chType outputType where fromChType  :: chType -> outputType
class IsChType chType => ToQueryPart chType           where toQueryPart :: chType -> BS.Builder




-- | ClickHouse Nullable(T) column type
-- (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

{-
This instance leads to disable -Wmissing-methods
Need to move it's semantics to another instances

instance {-# OVERLAPPING #-}
  ( TypeError
    (     'Text (ToChTypeName (Nullable (LowCardinality chType))) ':<>: 'Text " is unsupported type in ClickHouse."
    ':$$: 'Text "Use " ':<>: 'Text (ToChTypeName (LowCardinality (Nullable chType))) ':<>: 'Text " instead."
    )
  , IsChType chType
  ) => IsChType (Nullable (LowCardinality chType))
  where
  defaultValueOfTypeName = error "Unreachable"
  chTypeName = error "Unreachable"
-}

instance
  ( IsChType chType
  , KnownSymbol ("Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")")
  )
  =>
  IsChType (Nullable chType)
  where
  type ToChTypeName (Nullable chType) = NullableTypeName chType
  defaultValueOfTypeName = Nothing

instance
  ( ToQueryPart chType
  , IsChType (Nullable chType)
  )
  =>
  ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart

instance
  ( ToChType inputType chType
  , IsChType (Nullable inputType)
  )
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)

instance
  ( FromChType chType inputType
  , IsChType (Nullable chType)
  )
  =>
  FromChType (Nullable chType) (Nullable inputType)
  where
  fromChType = fmap (fromChType @chType)




-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
deriving newtype instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving newtype instance (Show chType, IsLowCardinalitySupported chType) => Show (LowCardinality chType)
deriving newtype instance (NFData chType, IsLowCardinalitySupported chType) => NFData (LowCardinality chType)
deriving newtype instance IsString (LowCardinality ChString)

class IsChType chType => IsLowCardinalitySupported chType
instance IsLowCardinalitySupported ChString
instance
  ( IsLowCardinalitySupported chType
  , IsChType (Nullable chType)
  ) =>
  IsLowCardinalitySupported (Nullable chType)

instance {-# OVERLAPPABLE #-}
  ( IsChType chType
  , TypeError
    (    'Text "LowCardinality("  ':<>: 'ShowType chType  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"
    ':$$: 'Text "  ChDateTime"
    ':$$: 'Text "  Nullable(T)"
    )
  ) => IsLowCardinalitySupported chType

instance
  ( IsLowCardinalitySupported chType
  , KnownSymbol ("LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")")
  ) =>
  IsChType (LowCardinality chType)
  where
  type ToChTypeName (LowCardinality chType) = "LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  defaultValueOfTypeName = MkLowCardinality $ defaultValueOfTypeName @chType

instance
  ( ToChType inputType chType
  , IsChType (LowCardinality inputType)
  , IsLowCardinalitySupported inputType
  )
  =>
  ToChType (LowCardinality inputType) chType
  where
  toChType = MkLowCardinality . toChType

instance IsLowCardinalitySupported chType => ToChType chType (LowCardinality chType)
  where
  toChType (MkLowCardinality value) = value

instance IsLowCardinalitySupported chType => FromChType chType (LowCardinality chType)
  where
  fromChType = MkLowCardinality

instance
  ( FromChType chType outputType
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  )
  =>
  FromChType (LowCardinality chType) outputType
  where
  fromChType (MkLowCardinality value) = fromChType value

instance
  ( ToQueryPart chType
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  )
  =>
  ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType




-- | ClickHouse UUID column type
newtype ChUUID = MkChUUID Word128
  deriving newtype (Generic, Show, Eq, NFData, Bounded, Prim, Enum)

instance IsChType ChUUID where
  type ToChTypeName ChUUID = "UUID"
  defaultValueOfTypeName = MkChUUID 0


instance ToChType ChUUID Word64 where toChType = MkChUUID . flip Word128 0
instance ToChType ChUUID (Word64, Word64) where toChType = MkChUUID . uncurry (flip Word128)

instance FromChType ChUUID (Word64, Word64) where fromChType (MkChUUID (Word128 w64hi w64lo)) = (w64hi, w64lo)




-- | ClickHouse String column type
newtype ChString = MkChString StrictByteString
  deriving newtype (Show, Eq, IsString, NFData)

instance IsChType ChString where
  type ToChTypeName ChString = "String"
  defaultValueOfTypeName = ""


instance ToQueryPart ChString where toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"

escapeQuery :: StrictByteString -> Builder
escapeQuery -- [ClickHaskell.DbTypes.ToDo.1]: Optimize
  = BS.byteString
  . BS8.concatMap (\case '\'' -> "\\\'"; '\\' -> "\\\\"; sym -> BS8.singleton sym;)

instance ToChType ChString StrictByteString where toChType = MkChString
instance ToChType ChString Builder          where toChType = MkChString . toStrict . toLazyByteString
instance ToChType ChString String           where toChType = MkChString . BS8.pack
instance ToChType ChString Text             where toChType = MkChString . Text.encodeUtf8
instance ToChType ChString Int              where toChType = MkChString . BS8.pack . show

instance FromChType ChString StrictByteString where fromChType (MkChString string) = string
instance
  ( TypeError
    (     'Text "ChString to Text using FromChType convertion could cause exception"
    ':$$: 'Text "Decode ByteString manually if you are sure it's always can be decoded or replace it with ByteString"
    )
  ) =>
  FromChType ChString Text
  where
  fromChType = error "Unreachable"




-- | ClickHouse Int8 column type
newtype ChInt8 = MkChInt8 Int8
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt8 where
  type ToChTypeName ChInt8 = "Int8"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt8
  where
  toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt8 Int8   where toChType = MkChInt8

instance FromChType ChInt8 Int8   where fromChType = coerce




-- | ClickHouse Int16 column type
newtype ChInt16 = MkChInt16 Int16
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt16 where
  type ToChTypeName ChInt16 = "Int16"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt16 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt16 Int16   where toChType = MkChInt16

instance FromChType ChInt16 Int16   where fromChType (MkChInt16 int16) = int16




-- | ClickHouse Int32 column type
newtype ChInt32 = MkChInt32 Int32
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt32 where
  type ToChTypeName ChInt32 = "Int32"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt32 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt32 Int32   where toChType = MkChInt32

instance FromChType ChInt32 Int32   where fromChType (MkChInt32 int32) = int32




-- | ClickHouse Int64 column type
newtype ChInt64 = MkChInt64 Int64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChInt64 where
  type ToChTypeName ChInt64 = "Int64"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt64 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt64 Int64   where toChType = MkChInt64 . fromIntegral
instance ToChType ChInt64 Int     where toChType = MkChInt64 . fromIntegral

instance FromChType ChInt64 Int64   where fromChType = coerce




-- | ClickHouse Int128 column type
newtype ChInt128 = MkChInt128 Int128
  deriving newtype (Show, Eq, Num, Prim, Bits, Ord, Real, Enum, Integral, Bounded, NFData)

instance IsChType ChInt128 where
  type ToChTypeName ChInt128 = "Int128"
  defaultValueOfTypeName = 0

instance ToQueryPart ChInt128 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChInt128 Int128   where toChType = MkChInt128 . fromIntegral

instance FromChType ChInt128 Int128   where fromChType (MkChInt128 int128) = int128




-- | ClickHouse UInt8 column type
newtype ChUInt8 = MkChUInt8 Word8
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt8 where
  type ToChTypeName ChUInt8 = "UInt8"
  defaultValueOfTypeName = 0


instance ToQueryPart ChUInt8 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt8 Word8   where toChType = MkChUInt8

instance FromChType ChUInt8 Word8   where fromChType (MkChUInt8 w8) = w8




-- | ClickHouse UInt16 column type
newtype ChUInt16 = MkChUInt16 Word16
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt16 where
  type ToChTypeName ChUInt16 = "UInt16"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt16 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt16 Word16   where toChType = coerce

instance FromChType ChUInt16 Word16   where fromChType = coerce




-- | ClickHouse UInt32 column type
newtype ChUInt32 = MkChUInt32 Word32
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt32 where
  type ToChTypeName ChUInt32 = "UInt32"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt32 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt32 Word32   where toChType = MkChUInt32

instance FromChType ChUInt32 Word32   where fromChType (MkChUInt32 word32) = word32




-- | ClickHouse UInt64 column type
newtype ChUInt64 = MkChUInt64 Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt64 where
  type ToChTypeName ChUInt64 = "UInt64"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt64 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt64 Word64   where toChType = MkChUInt64

instance FromChType ChUInt64 Word64   where fromChType (MkChUInt64 w64) = w64




-- | ClickHouse UInt128 column type
newtype ChUInt128 = MkChUInt128 Word128
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChUInt128 where
  type ToChTypeName ChUInt128 = "UInt128"
  defaultValueOfTypeName = 0

instance ToQueryPart ChUInt128 where toQueryPart = BS.byteString . BS8.pack . show

instance ToChType ChUInt128 Word128   where toChType = MkChUInt128
instance ToChType ChUInt128 Word64    where toChType = MkChUInt128 . fromIntegral

instance FromChType ChUInt128 Word128   where fromChType (MkChUInt128 w128) = w128




-- | ClickHouse DateTime column type
newtype ChDateTime = MkChDateTime Word32
  deriving newtype (Show, Eq, Prim, Num, Bits, Enum, Ord, Real, Integral, Bounded, NFData)

instance IsChType ChDateTime
  where
  type ToChTypeName ChDateTime = "DateTime"
  defaultValueOfTypeName = MkChDateTime 0

instance ToQueryPart ChDateTime
  where
  toQueryPart chDateTime = let time = BS8.pack . show . fromChType @ChDateTime @Word32 $ chDateTime
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)

instance ToChType ChDateTime Word32     where toChType = MkChDateTime
instance ToChType ChDateTime UTCTime    where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType ChDateTime ZonedTime  where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC

instance FromChType ChDateTime Word32     where fromChType = coerce
instance FromChType ChDateTime UTCTime    where fromChType (MkChDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)




newtype ChDate = MkChDate Word16
  deriving newtype (Show, Eq, Prim, Bits, Bounded, Enum, NFData)

instance IsChType ChDate where
  type ToChTypeName ChDate = "Date"
  defaultValueOfTypeName = MkChDate 0

instance ToChType ChDate Word16 where toChType = MkChDate

instance FromChType ChDate Word16 where fromChType = coerce




newtype ChArray a = MkChArray [a]
  deriving newtype (Show, Eq, NFData)

instance
  ( IsChType chType
  , KnownSymbol (AppendSymbol (AppendSymbol "Array(" (ToChTypeName chType)) ")")
  ) =>
  IsChType (ChArray chType)
  where
  type ToChTypeName (ChArray chType) = "Array(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  defaultValueOfTypeName = MkChArray []




instance
  ( ToQueryPart chType
  , IsChType (ChArray chType)
  ) =>
  ToQueryPart (ChArray chType)
  where
  toQueryPart
    = (\x -> "[" <> x <> "]")
    . (maybe "" (uncurry (foldr (\ a b -> a <> "," <> b)))
    . uncons
    . map (toQueryPart @chType))
    . fromChType

instance
  ( IsChType chType
  , IsChType (ChArray chType)
  ) =>
  FromChType (ChArray chType) [chType] where fromChType (MkChArray values) = values

instance
  ( ToChType chType inputType
  , IsChType (ChArray chType)
  ) =>
  ToChType (ChArray chType) [inputType] where toChType = MkChArray . map toChType




{- |
  Unsigned variable-length quantity encoding
  
  Part of protocol implementation
-}
newtype UVarInt = MkUVarInt Word64
  deriving newtype (Show, Eq, Num, Prim, Bits, Enum, Ord, Real, Integral, Bounded, NFData)




-- * Columns

data Columns (columns :: [Type]) where
  Empty :: Columns '[]
  AddColumn
    :: KnownColumn (Column name chType)
    => Column name chType
    -> Columns columns
    -> Columns (Column name chType ': columns)

{- |
Column declaration

For example:

@
type MyColumn = Column "myColumn" ChString
@
-}
data Column (name :: Symbol) (chType :: Type) where
  RegularColumn :: IsChType chType => UVarInt -> [chType] -> Column name chType
  NullableColumn :: IsChType chType => UVarInt -> [chType] -> Column name chType

type family GetColumnName column :: Symbol
  where
  GetColumnName (Column name columnType) = name

type family GetColumnType column :: Type
  where
  GetColumnType (Column name columnType) = columnType

class
  ( IsChType (GetColumnType column)
  , KnownSymbol (GetColumnName column)
  ) =>
  KnownColumn column where
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName column)) Proxy

  renderColumnType :: Builder
  renderColumnType = chTypeName @(GetColumnType column)

  mkColumn :: UVarInt -> [GetColumnType column] -> Column (GetColumnName column) (GetColumnType column)

{-# INLINE [0] columnSize #-}
columnSize :: Column name chType -> UVarInt
columnSize column = case column of
  (RegularColumn size _listValues) -> size
  (NullableColumn size _nullableValues) -> size

{-# INLINE [0] columnValues #-}
columnValues :: Column name chType -> [chType]
columnValues column = case column of
  (RegularColumn _size values) -> values
  (NullableColumn _size nullableValues) -> nullableValues

instance KnownSymbol name => KnownColumn (Column name ChUInt8) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChUInt16) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChUInt32) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChUInt64) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChUInt128) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChInt8)  where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChInt16) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChInt32) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChInt64) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChInt128) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChDate) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChDateTime) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChUUID) where mkColumn = RegularColumn
instance
  ( KnownSymbol name
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType)) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name ChString) where mkColumn = RegularColumn
instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType)) where mkColumn = RegularColumn
instance KnownSymbol name => KnownColumn (Column name (ChArray ChString)) where mkColumn = RegularColumn

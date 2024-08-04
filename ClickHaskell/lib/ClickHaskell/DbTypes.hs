{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , LambdaCase
  , OverloadedStrings
  , StandaloneDeriving
  , UndecidableInstances
#-}

{-# OPTIONS_GHC
  -Wno-missing-methods
#-}

module ClickHaskell.DbTypes
( IsChType(ToChTypeName, chTypeName, IsWriteOptional)
, ToChType(toChType)
, FromChType(fromChType)
, Serializable(serialize)
, Deserializable(deserialize)
, ToQueryPart(toQueryPart)

, ChDateTime

, ChInt8
, ChInt16
, ChInt32
, ChInt64
, ChInt128, Int128

, ChUInt8
, ChUInt16
, ChUInt32
, ChUInt64
, ChUInt128, Word128

, ChString
, ChUUID

, ChArray
, Nullable
, LowCardinality, IsLowCardinalitySupported
) where


-- External
import Data.UUID     as UUID (UUID, fromASCIIBytes, toASCIIBytes, toWords64, fromWords64)
import Data.WideWord (Int128, Word128(Word128))


-- GHC included
import Control.DeepSeq         (NFData)
import Data.ByteString         as BS (StrictByteString, take, empty, drop)
import Data.ByteString.Builder as BS (Builder, byteString)
import Data.ByteString.Char8   as BS8 (concatMap, pack, readInt, readInteger, singleton, unpack, replicate, length, break)
import Data.Coerce             (coerce)
import Data.Maybe              (fromJust)
import Data.Int                (Int32, Int16, Int8, Int64)
import Data.Text               as Text (Text)
import Data.Text.Encoding      as Text (encodeUtf8)
import Data.Time
  ( parseTimeM
  , UTCTime, defaultTimeLocale
  , ZonedTime, zonedTimeToUTC
  )
import Data.Time.Clock.POSIX         (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Typeable                 (Proxy(..))
import Data.List                     (uncons)
import Data.String                   (IsString)
import Data.Vector.Primitive.Mutable (Prim)
import Data.Word                     (Word64, Word32, Word16, Word8)
import GHC.TypeLits                  (AppendSymbol, ErrorMessage (..), Symbol, TypeError, KnownSymbol, symbolVal)


class
  IsChType chType
  where
  -- | Shows database original type name
  --
  -- @
  -- type ToChTypeName ChString = \"String\"
  -- type ToChTypeName (Nullable ChUInt32) = \"Nullable(UInt32)\"
  -- @
  type ToChTypeName chType :: Symbol

  chTypeName :: KnownSymbol (ToChTypeName chType) => StrictByteString
  chTypeName = BS8.pack $ symbolVal (Proxy @(ToChTypeName chType))

  -- |
  -- There is only one native ClickHaskell write optional type - Nullable(T)
  --
  -- @
  -- type IsWriteOptional (Nullable someChType) = True
  -- @
  type IsWriteOptional chType :: Bool

class
  IsChType chType
  =>
  Serializable chType
  where
  serialize :: chType -> Builder

class
  IsChType chType
  =>
  Deserializable chType
  where
  deserialize :: StrictByteString -> chType

class
  IsChType chType
  =>
  ToChType chType inputType
  where
  toChType :: inputType -> chType

class
  IsChType chType
  =>
  FromChType chType outputType
  where
  fromChType :: chType -> outputType

class
  IsChType chType
  =>
  ToQueryPart chType
  where
  toQueryPart :: chType -> BS.Builder








-- | ClickHouse Nullable(T) column type
-- (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

instance {-# OVERLAPPING #-}
  ( TypeError
    (     'Text (ToChTypeName (Nullable (LowCardinality chType))) ':<>: 'Text " is unsupported type in ClickHouse."
    ':$$: 'Text "Use " ':<>: 'Text (ToChTypeName (LowCardinality (Nullable chType))) ':<>: 'Text " instead."
    )
  , IsChType chType
  ) => IsChType (Nullable (LowCardinality chType))

instance
  IsChType chType
  =>
  IsChType (Nullable chType)
  where
  type ToChTypeName (Nullable chType) = NullableTypeName chType
  type IsWriteOptional (Nullable _)   = 'True

instance
  Deserializable chType
  =>
  Deserializable (Nullable chType)
  where
  deserialize "\\N" = Nothing
  deserialize someTypeBs = Just (deserialize someTypeBs)

instance
  Serializable chType
  =>
  Serializable (Nullable chType)
  where
  serialize = maybe "\\N" serialize

instance
  ToQueryPart chType
  =>
  ToQueryPart (Nullable chType)
  where
  toQueryPart = maybe "null" toQueryPart

instance
  ToChType inputType chType
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)

instance
  FromChType chType inputType
  =>
  FromChType (Nullable chType) (Nullable inputType)
  where
  fromChType = fmap (fromChType @chType)








-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = MkLowCardinality chType
deriving instance (Eq chType, IsLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving instance (Show chType, IsLowCardinalitySupported chType) => Show (LowCardinality chType)
deriving instance (NFData chType, IsLowCardinalitySupported chType) => NFData (LowCardinality chType)
deriving newtype instance IsString (LowCardinality ChString)

class
  IsChType chType
  =>
  IsLowCardinalitySupported chType

instance IsLowCardinalitySupported ChString
instance IsLowCardinalitySupported chType => IsLowCardinalitySupported (Nullable chType)

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
  IsLowCardinalitySupported chType
  =>
  IsChType (LowCardinality chType)
  where
  type ToChTypeName (LowCardinality chType) = "LowCardinality(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  type IsWriteOptional (LowCardinality chType) = IsWriteOptional chType

instance
  ( Serializable chType
  , IsLowCardinalitySupported chType
  ) =>
  Serializable (LowCardinality chType)
  where
  serialize (MkLowCardinality value) = serialize value

instance
  ( Deserializable chType
  , IsLowCardinalitySupported chType
  ) =>
  Deserializable (LowCardinality chType)
  where
  deserialize = MkLowCardinality . deserialize

instance
  ( ToChType inputType chType
  , IsLowCardinalitySupported inputType
  ) =>
  ToChType (LowCardinality inputType) chType
  where
  toChType = MkLowCardinality . toChType

instance
  ( IsLowCardinalitySupported chType
  , IsChType chType
  ) =>
  ToChType chType (LowCardinality chType)
  where
  toChType (MkLowCardinality value) = value

instance
  ( IsLowCardinalitySupported chType
  , IsChType chType
  ) =>
  FromChType chType (LowCardinality chType)
  where
  fromChType = MkLowCardinality

instance
  ( FromChType chType outputType
  , IsLowCardinalitySupported chType
  ) =>
  FromChType (LowCardinality chType) outputType
  where
  fromChType (MkLowCardinality value) = fromChType value

instance
  ( ToQueryPart chType
  , IsLowCardinalitySupported chType
  ) =>
  ToQueryPart (LowCardinality chType)
  where
  toQueryPart (MkLowCardinality chType) = toQueryPart chType








-- | ClickHouse UUID column type
newtype ChUUID = MkChUUID UUID
  deriving newtype (Show, Eq, NFData)

instance IsChType ChUUID
  where
  type ToChTypeName    ChUUID = "UUID"
  type IsWriteOptional ChUUID = 'False

instance Serializable ChUUID
  where
  serialize (MkChUUID uuid) = (BS.byteString . UUID.toASCIIBytes) uuid

instance Deserializable ChUUID
  where
  deserialize = MkChUUID . fromJust . UUID.fromASCIIBytes

instance ToChType ChUUID ChUUID where toChType = id
instance ToChType ChUUID UUID   where toChType = MkChUUID
instance ToChType ChUUID Word64 where toChType = MkChUUID . UUID.fromWords64 0 . fromIntegral

instance FromChType ChUUID ChUUID where fromChType = id
instance FromChType ChUUID UUID   where fromChType (MkChUUID uuid) = uuid








-- | ClickHouse String column type
newtype ChString = MkChString StrictByteString
  deriving newtype (Show, Eq, IsString, NFData)

instance IsChType ChString
  where
  type ToChTypeName    ChString = "String"
  type IsWriteOptional ChString = 'False

instance Serializable ChString
  where
  serialize (MkChString string) = (BS.byteString . escape) string

escape :: StrictByteString -> StrictByteString
escape -- [ClickHaskell.DbTypes.ToDo.2]: Optimize
  = BS8.concatMap
    (\case
      '\t' -> "\\t"
      '\n' -> "\\n"
      sym -> BS8.singleton sym
    )

instance Deserializable ChString
  where
  deserialize = MkChString . deescape

-- There are a big trade off between safity and performance
-- Corner case strings with a lot of escaped symbols would reduce deserialization speed
-- ToDo: rewrite (de)serialization to work via binary clickhouse formats
deescape :: StrictByteString -> StrictByteString
deescape bs = case BS8.break (=='\\') bs of
  (beforeEscaping, startWithEscaping) ->
    if BS.empty == startWithEscaping
    then bs
    else case BS.take 2 startWithEscaping of
      "\\b" -> beforeEscaping <> "\b" <> BS.drop 2 startWithEscaping
      "\\t" -> beforeEscaping <> "\t" <> BS.drop 2 startWithEscaping
      "\\n" -> beforeEscaping <> "\n" <> BS.drop 2 startWithEscaping
      "\\f" -> beforeEscaping <> "\f" <> BS.drop 2 startWithEscaping
      "\\r" -> beforeEscaping <> "\r" <> BS.drop 2 startWithEscaping
      "\\'" -> beforeEscaping <> "'" <> BS.drop 2 startWithEscaping
      "\\\\" -> beforeEscaping <> "\\" <> BS.drop 2 startWithEscaping
      _ -> bs

instance ToQueryPart ChString
  where
  toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"

escapeQuery :: StrictByteString -> Builder
escapeQuery -- [ClickHaskell.DbTypes.ToDo.1]: Optimize
  = BS.byteString
  . BS8.concatMap
    (\case
      '\'' -> "\\\'"
      '\\' -> "\\\\"
      sym -> BS8.singleton sym
    )

instance ToChType ChString ChString         where toChType = id
instance ToChType ChString StrictByteString where toChType = MkChString
instance ToChType ChString String           where toChType = MkChString . BS8.pack
instance ToChType ChString Text             where toChType = MkChString . Text.encodeUtf8
instance ToChType ChString Int              where toChType = MkChString . BS8.pack . show

instance FromChType ChString ChString         where fromChType = id
instance FromChType ChString StrictByteString where fromChType (MkChString string) = string
instance
  ( TypeError
    (     'Text "You are trying to convert ChString to Text using FromChType convertion mechanism"
    ':$$: 'Text "It could be a bad idea since Text is semantically smaller than ByteString"
    ':$$: 'Text "Decode ByteString manually if you are sure it's always can be decoded or replace it with ByteString"
    )
  ) =>
  FromChType ChString Text








-- | ClickHouse Int8 column type
newtype ChInt8 = MkChInt8 Int8
  deriving newtype (Show, Eq, Num, Prim, Bounded, NFData)

instance IsChType ChInt8
  where
  type ToChTypeName    ChInt8 = "Int8"
  type IsWriteOptional ChInt8 = 'False

instance Serializable ChInt8
  where
  serialize = BS.byteString . BS8.pack . show @ChInt8 . coerce

instance Deserializable ChInt8
  where
  deserialize = MkChInt8 . fromIntegral . fst . fromJust . BS8.readInt

instance ToQueryPart ChInt8
  where
  toQueryPart = serialize

instance ToChType ChInt8 ChInt8 where toChType = id
instance ToChType ChInt8 Int8   where toChType = MkChInt8

instance FromChType ChInt8 ChInt8 where fromChType = id
instance FromChType ChInt8 Int8   where fromChType = coerce








-- | ClickHouse Int16 column type
newtype ChInt16 = MkChInt16 Int16
  deriving newtype (Show, Eq, Num, Prim, Bounded, NFData)

instance IsChType ChInt16
  where
  type ToChTypeName    ChInt16 = "Int16"
  type IsWriteOptional ChInt16 = 'False

instance Serializable ChInt16
  where
  serialize = BS.byteString . BS8.pack . show @Int16 . coerce

instance Deserializable ChInt16
  where
  deserialize  = MkChInt16 . fromIntegral . fst . fromJust . BS8.readInt

instance ToQueryPart ChInt16
  where
  toQueryPart = serialize

instance ToChType ChInt16 ChInt16 where toChType = id
instance ToChType ChInt16 Int16   where toChType = MkChInt16

instance FromChType ChInt16 ChInt16 where fromChType = id
instance FromChType ChInt16 Int16   where fromChType (MkChInt16 int16) = int16








-- | ClickHouse Int32 column type
newtype ChInt32 = MkChInt32 Int32
  deriving newtype (Show, Eq, Num, Prim, Bounded, Enum, NFData)

instance IsChType ChInt32
  where
  type ToChTypeName    ChInt32 = "Int32"
  type IsWriteOptional ChInt32 = 'False

instance Serializable ChInt32
  where
  serialize = BS.byteString . BS8.pack . show @ChInt32 . coerce

instance Deserializable ChInt32
  where
  deserialize = MkChInt32 . fromIntegral . fst . fromJust . BS8.readInt

instance ToQueryPart ChInt32
  where
  toQueryPart = serialize

instance ToChType ChInt32 ChInt32 where toChType = id
instance ToChType ChInt32 Int32   where toChType = MkChInt32

instance FromChType ChInt32 ChInt32 where fromChType = id
instance FromChType ChInt32 Int32   where fromChType (MkChInt32 int32) = int32








-- | ClickHouse Int64 column type
newtype ChInt64 = MkChInt64 Int64
  deriving newtype (Show, Eq, Num, Prim, Bounded, Enum, NFData)

instance IsChType ChInt64
  where
  type ToChTypeName    ChInt64 = "Int64"
  type IsWriteOptional ChInt64 = 'False

instance Serializable ChInt64
  where
  serialize = BS.byteString . BS8.pack . show @Int64 . coerce

instance Deserializable ChInt64
  where
  deserialize = MkChInt64 . fromInteger . fst . fromJust . BS8.readInteger

instance ToQueryPart ChInt64
  where
  toQueryPart = serialize

instance ToChType ChInt64 ChInt64 where toChType = id
instance ToChType ChInt64 Int64   where toChType = MkChInt64 . fromIntegral
instance ToChType ChInt64 Int     where toChType = MkChInt64 . fromIntegral

instance FromChType ChInt64 ChInt64 where fromChType = id
instance FromChType ChInt64 Int64   where fromChType = coerce








-- | ClickHouse Int128 column type
newtype ChInt128 = MkChInt128 Int128
  deriving newtype (Show, Eq, Num, Prim, Bounded, NFData)

instance IsChType ChInt128
  where
  type ToChTypeName    ChInt128 = "Int128"
  type IsWriteOptional ChInt128 = 'False

instance Serializable ChInt128
  where
  serialize = BS.byteString . BS8.pack . show @ChInt128 . coerce

instance Deserializable ChInt128
  where
  deserialize = MkChInt128 . fromInteger . fst . fromJust . BS8.readInteger

instance ToQueryPart ChInt128
  where
  toQueryPart = serialize

instance ToChType ChInt128 ChInt128 where toChType = id
instance ToChType ChInt128 Int128   where toChType = MkChInt128 . fromIntegral

instance FromChType ChInt128 ChInt128 where fromChType = id
instance FromChType ChInt128 Int128   where fromChType (MkChInt128 int128) = int128








-- | ClickHouse UInt8 column type
newtype ChUInt8 = MkChUInt8 Word8
  deriving newtype (Show, Eq, Num, Prim, Bounded, NFData)

instance IsChType ChUInt8
  where
  type ToChTypeName    ChUInt8 = "UInt8"
  type IsWriteOptional ChUInt8 = 'False

instance Serializable ChUInt8
  where
  serialize = BS.byteString. BS8.pack . show @ChUInt8 . coerce

instance Deserializable ChUInt8
  where
  deserialize = MkChUInt8 . fromIntegral . fst . fromJust . BS8.readInt

instance ToQueryPart ChUInt8
  where
  toQueryPart = serialize

instance ToChType ChUInt8 ChUInt8 where toChType = id
instance ToChType ChUInt8 Word8   where toChType = MkChUInt8

instance FromChType ChUInt8 ChUInt8 where fromChType = id
instance FromChType ChUInt8 Word8   where fromChType (MkChUInt8 word8) = word8








-- | ClickHouse UInt16 column type
newtype ChUInt16 = MkChUInt16 Word16
  deriving newtype (Show, Eq, Num, Prim, Bounded, NFData)

instance IsChType ChUInt16
  where
  type ToChTypeName    ChUInt16 = "UInt16"
  type IsWriteOptional ChUInt16 = 'False

instance Serializable ChUInt16
  where
  serialize = BS.byteString . BS8.pack . show @ChUInt16 . coerce

instance Deserializable ChUInt16
  where
  deserialize = MkChUInt16 . fromIntegral . fst . fromJust . BS8.readInt

instance ToQueryPart ChUInt16
  where
  toQueryPart = serialize

instance ToChType ChUInt16 ChUInt16 where toChType = id
instance ToChType ChUInt16 Word16   where toChType = coerce

instance FromChType ChUInt16 ChUInt16 where fromChType = id
instance FromChType ChUInt16 Word16   where fromChType = coerce








-- | ClickHouse UInt32 column type
newtype ChUInt32 = MkChUInt32 Word32
  deriving newtype (Show, Eq, Num, Prim, Bounded, Enum, NFData)

instance IsChType ChUInt32
  where
  type ToChTypeName    ChUInt32 = "UInt32"
  type IsWriteOptional ChUInt32 = 'False

instance Serializable ChUInt32
  where
  serialize (MkChUInt32 word32) = (BS.byteString . BS8.pack . show) word32

instance Deserializable ChUInt32
  where
  deserialize = MkChUInt32 . fromIntegral . fst . fromJust . BS8.readInt

instance ToQueryPart ChUInt32
  where
  toQueryPart = serialize

instance ToChType ChUInt32 ChUInt32 where toChType = id
instance ToChType ChUInt32 Word32   where toChType = MkChUInt32

instance FromChType ChUInt32 ChUInt32 where fromChType = id
instance FromChType ChUInt32 Word32   where fromChType (MkChUInt32 word32) = word32








-- | ClickHouse UInt64 column type
newtype ChUInt64 = MkChUInt64 Word64
  deriving newtype (Show, Eq, Num, Prim, Bounded, Enum, NFData)

instance IsChType ChUInt64
  where
  type ToChTypeName    ChUInt64 = "UInt64"
  type IsWriteOptional ChUInt64 = 'False

instance Serializable ChUInt64
  where
  serialize = BS.byteString . BS8.pack . show @ChUInt64 . coerce

instance Deserializable ChUInt64
  where
  deserialize = MkChUInt64 . fromIntegral . fst . fromJust . BS8.readInteger

instance ToQueryPart ChUInt64
  where
  toQueryPart = serialize

instance ToChType ChUInt64 ChUInt64 where toChType = id
instance ToChType ChUInt64 Word64   where toChType = MkChUInt64

instance FromChType ChUInt64 ChUInt64 where fromChType = id
instance FromChType ChUInt64 Word64   where fromChType (MkChUInt64 w64) = w64








-- | ClickHouse UInt128 column type
newtype ChUInt128 = MkChUInt128 Word128
  deriving newtype (Show, Eq, Num, Prim, Bounded, Enum, NFData)

instance IsChType ChUInt128
  where
  type ToChTypeName    ChUInt128 = "UInt128"
  type IsWriteOptional ChUInt128 = 'False

instance Serializable ChUInt128
  where
  serialize = BS.byteString . BS8.pack . show @ChUInt128 . coerce

instance Deserializable ChUInt128
  where
  deserialize = MkChUInt128 . fromIntegral . fst . fromJust . BS8.readInteger

instance ToQueryPart ChUInt128
  where
  toQueryPart = serialize

instance ToChType ChUInt128 ChUInt128 where toChType = id
instance ToChType ChUInt128 Word128   where toChType = MkChUInt128
instance ToChType ChUInt128 UUID      where toChType = MkChUInt128 . uncurry Word128 . toWords64
instance ToChType ChUInt128 Word64    where toChType = MkChUInt128 . fromIntegral

instance FromChType ChUInt128 ChUInt128 where fromChType = id
instance FromChType ChUInt128 Word128   where fromChType (MkChUInt128 w128) = w128








-- | ClickHouse DateTime column type
newtype ChDateTime = MkChDateTime Word32
  deriving newtype (Show, Eq, Prim, Bounded, Enum, NFData)

instance IsChType ChDateTime
  where
  type ToChTypeName    ChDateTime = "DateTime"
  type IsWriteOptional ChDateTime = 'False

instance Serializable ChDateTime where
  serialize (MkChDateTime w32)
    = let time = BS8.pack $ show w32
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)

instance Deserializable ChDateTime where
  deserialize
    = MkChDateTime . fromInteger
    . floor . utcTimeToPOSIXSeconds
    . fromJust . parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    . BS8.unpack

instance ToQueryPart ChDateTime
  where
  toQueryPart = serialize

instance ToChType ChDateTime ChDateTime where toChType = id
instance ToChType ChDateTime Word32     where toChType = MkChDateTime
instance ToChType ChDateTime UTCTime    where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType ChDateTime ZonedTime  where toChType = MkChDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC

instance FromChType ChDateTime ChDateTime where fromChType = id
instance FromChType ChDateTime Word32     where fromChType = coerce
instance FromChType ChDateTime UTCTime    where fromChType (MkChDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)








newtype ChDate = MkChDate Word16
  deriving newtype (Show, Eq, Prim, Bounded, Enum, NFData)

instance IsChType ChDate
  where
  type ToChTypeName    ChDate = "Date"
  type IsWriteOptional ChDate = 'False








newtype ChArray a = MkChArray [a]
  deriving newtype (Show, Eq, NFData)

instance IsChType chType => IsChType (ChArray chType)
  where
  type ToChTypeName    (ChArray chType) = "Array(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
  type IsWriteOptional (ChArray chType) = 'False

instance Deserializable (ChArray ChString) where
  deserialize = undefined

instance ToQueryPart chType => ToQueryPart (ChArray chType)
  where
  toQueryPart = (\x -> "[" <> x <> "]") . (maybe "" (uncurry (foldr (\ a b -> a <> "," <> b))) . uncons . map (toQueryPart @chType)) . fromChType 

instance IsChType chType => FromChType (ChArray chType) [chType] where fromChType (MkChArray values) = values

instance IsChType chType           => ToChType (ChArray chType) [chType] where toChType = MkChArray
instance ToChType chType inputType => ToChType (ChArray chType) [inputType] where toChType = MkChArray . map toChType

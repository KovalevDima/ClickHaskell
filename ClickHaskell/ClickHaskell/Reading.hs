{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module ClickHaskell.Reading
  ( ReadableFrom(..)
  , GReadable(..)
  , Deserializable(..)
  ) where


-- Internal dependencies
import ClickHaskell.DbTypes
    ( Nullable
    , LowCardinality, IsLowCardinalitySupported
    , ChUInt8, ChUInt16, ChUInt32, ChUInt64, ChUInt128
    , ChInt8, ChInt16, ChInt32, ChInt64, ChInt128
    , ChString
    , ChUUID
    , ChDateTime
    , ToChType(..)
    , FromChType(..)
    )
import ClickHaskell.Tables (CompiledColumn(..), HasColumns(..), TakeColumn)


-- GHC included
import Data.ByteString         as BS (StrictByteString, drop, take, empty)
import Data.ByteString.Char8   as BS8 (span, readInt, readInteger, break, unpack)
import Data.ByteString.Builder as BS (Builder)
import Data.Kind               (Type)
import GHC.Generics            (K1(..), M1(..), type (:*:)(..), Rec0, D1, C1, S1, Meta(MetaSel), Generic (..))
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time (parseTimeM, defaultTimeLocale)
import qualified Data.UUID as UUID
import Data.Int (Int16, Int8, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

-- External
import Data.WideWord

-- * Reading

class
  ( HasColumns hasColumns
  , GReadable (GetColumns hasColumns) (Rep record)
  ) =>
  ReadableFrom hasColumns record
  where

  default fromTsvLine :: (Generic record) => StrictByteString -> record
  fromTsvLine :: StrictByteString -> record
  fromTsvLine = to . gFromTsvBs @(GetColumns hasColumns)

  default readingColumns :: (Generic record) => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)

class GReadable
  (columns :: [Type])
  f
  where
  gFromTsvBs :: StrictByteString -> f p
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  gFromTsvBs = M1 . M1 . gFromTsvBs @columns
  gReadingColumns = gReadingColumns @columns @f


instance
  GReadable columns (left1 :*: (left2 :*: right))
  =>
  GReadable columns ((left1 :*: left2) :*: right)
  where
  gFromTsvBs bs =
    let (left1 :*: (left2 :*: right)) = gFromTsvBs @columns bs
    in ((left1 :*: left2) :*: right)
  gReadingColumns = gReadingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( CompiledColumn column
  , '(column, restColumns) ~ TakeColumn selectorName columns
  , FromChType (GetColumnType column) inputType
  , Deserializable (GetColumnType column)
  , GReadable restColumns right
  ) => GReadable columns (S1 (MetaSel (Just selectorName) a b f) (Rec0 inputType) :*: right)
  where
  gFromTsvBs bs = 
    let (beforeTab, afterTab) = BS8.span (/= '\t') bs 
    in 
    (M1 . K1 . fromChType @(GetColumnType column) . deserialize $ beforeTab) :*: gFromTsvBs @restColumns @right (BS.drop 1 afterTab)
  gReadingColumns = renderColumnName @column <> ", " <> gReadingColumns @restColumns @right

instance
  ( CompiledColumn column
  , '(column, restColumns) ~ TakeColumn selectorName columns
  , Deserializable (GetColumnType column)
  , FromChType (GetColumnType column) inputType
  ) => GReadable columns ((S1 (MetaSel (Just selectorName) a b f)) (Rec0 inputType))
  where
  gFromTsvBs = M1 . K1 . fromChType @(GetColumnType column) . deserialize
  gReadingColumns = renderColumnName @column




-- * Deserialization

class
  Deserializable chType
  where
  deserialize :: StrictByteString -> chType

instance
  Deserializable chType
  =>
  Deserializable (Nullable chType)
  where
  deserialize "\\N" = Nothing
  deserialize someTypeBs = Just (deserialize someTypeBs)

instance
  ( Deserializable chType
  , ToChType chType chType
  , IsLowCardinalitySupported chType
  ) =>
  Deserializable (LowCardinality chType)
  where
  deserialize = toChType @(LowCardinality chType) @chType . deserialize

instance Deserializable ChUUID
  where
  deserialize = toChType . fromJust . UUID.fromASCIIBytes

instance Deserializable ChString
  where
  deserialize = toChType . deescape

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

instance Deserializable ChInt8
  where
  deserialize = toChType @ChInt8 @Int8 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChInt16
  where
  deserialize  = toChType @ChInt16 @Int16 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChInt32
  where
  deserialize = toChType @ChInt32 @Int32 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChInt64
  where
  deserialize = toChType @ChInt64 @Int64 . fromInteger . fst . fromJust . BS8.readInteger

instance Deserializable ChInt128
  where
  deserialize = toChType @ChInt128 @Int128 . fromInteger . fst . fromJust . BS8.readInteger

instance Deserializable ChUInt8
  where
  deserialize = toChType @ChUInt8 @Word8 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChUInt16
  where
  deserialize = toChType @ChUInt16 @Word16 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChUInt32
  where
  deserialize = toChType @ChUInt32 @Word32 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChUInt64
  where
  deserialize = toChType @ChUInt64 @Word64 . fromIntegral . fst . fromJust . BS8.readInteger

instance Deserializable ChUInt128
  where
  deserialize = toChType @ChUInt128 @Word128 . fromIntegral . fst . fromJust . BS8.readInteger

instance Deserializable ChDateTime where
  deserialize
    = toChType @ChDateTime @Word32 . fromInteger
    . floor . utcTimeToPOSIXSeconds
    . fromJust . parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    . BS8.unpack

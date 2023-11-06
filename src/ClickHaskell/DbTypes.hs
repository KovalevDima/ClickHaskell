{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , StandaloneDeriving
  , UndecidableInstances
  #-}

module ClickHaskell.DbTypes
  ( IsChType(ToChTypeName), Serializable(serialize), Deserializable(deserialize)
  , QuerySerializable(renderForQuery)
  , toCh, ToChType(toChType), FromChType(fromChType)

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

  , ChString
  , ChUUID, nilChUUID

  , Nullable
  , LowCardinality

  , Word32, Word64, Text, Int64
  ) where

-- External dependencies
import Data.UUID     as UUID (UUID, fromASCIIBytes, toASCIIBytes, nil)
import Data.WideWord (Int128)

-- GHC included libraries imports
import Data.ByteString       as BS (ByteString)
import Data.ByteString.Char8 as BS8 (concatMap, pack, readInt, readInteger, singleton, unpack, replicate, length)
import Data.Coerce           (coerce)
import Data.Maybe            (fromJust)
import Data.Int              (Int32, Int16, Int8, Int64)
import Data.Kind             (Type)
import Data.Text             as Text (Text)
import Data.Text.Encoding    as Text (encodeUtf8)
import Data.Time             (UTCTime, defaultTimeLocale, nominalDiffTimeToSeconds, parseTimeM, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.String           (IsString)
import Data.Word             (Word64, Word32, Word16, Word8)
import GHC.TypeLits          (AppendSymbol, ErrorMessage (..), Symbol, TypeError)


class IsChType chType
  where
  type ToChTypeName chType :: Symbol

class IsChType chType => Serializable   chType where serialize   :: chType -> BS.ByteString
class IsChType chType => Deserializable chType where deserialize :: BS.ByteString -> chType

class Serializable chType => QuerySerializable chType where renderForQuery :: chType -> BS.ByteString
instance {-# OVERLAPPING #-} QuerySerializable ChString where renderForQuery (ChString bs) = "'" <> bs <> "'"
instance Serializable chType => QuerySerializable chType where renderForQuery = serialize
instance {-# OVERLAPPING #-} QuerySerializable (LowCardinality ChString) where renderForQuery (LowCardinality chString) = renderForQuery chString
instance {-# OVERLAPPING #-} QuerySerializable (Nullable ChString) where renderForQuery = maybe "\\N" renderForQuery


toCh :: forall inputType chType . (ToChType chType inputType) => inputType -> chType
toCh = toChType @chType @inputType

class IsChType chType => ToChType chType inputType
  where
  toChType :: inputType -> chType

class IsChType chType => FromChType chType outputType
  where
  fromChType :: chType -> outputType




-- | ClickHouse Nullable(T) column type (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"

instance IsChType chType => IsChType (Nullable chType) where
  type ToChTypeName (Nullable chType) = NullableTypeName chType

instance
  ( Deserializable chType
  ) => Deserializable (Nullable chType)
  where 
  deserialize "\\N" = Nothing
  deserialize someTypeBs = Just (deserialize someTypeBs)  

instance
  ( Serializable chType
  ) => Serializable (Nullable chType)
  where
  serialize = maybe "\\N" serialize

instance
  ( ToChType inputType chType
  ) => ToChType (Nullable inputType) (Nullable chType) where
    toChType Nothing  = Nothing
    toChType (Just a) = Just (toChType @inputType @chType a)

instance
  ( FromChType chType inputType
  ) => FromChType (Nullable chType) (Nullable inputType) where
    fromChType Nothing  = Nothing
    fromChType (Just a) = Just (fromChType @chType a)




-- | ClickHouse LowCardinality(T) column type
newtype LowCardinality chType = LowCardinality (ToLowCardinalitySupported chType)
deriving instance Eq (ToLowCardinalitySupported chType) => Eq (LowCardinality chType)
deriving instance Show (ToLowCardinalitySupported chType) => Show (LowCardinality chType)

instance IsChType (LowCardinality chType) where
  type ToChTypeName (LowCardinality chType) =
    "LowCardinality(" `AppendSymbol` ToChTypeName (ToLowCardinalitySupported chType) `AppendSymbol` ")"

type family ToLowCardinalitySupported a :: Type where
  ToLowCardinalitySupported (Nullable (LowCardinality chType)) =  TypeError
    (    'Text "Nullable(LowCardinality("     ':<>: 'Text (ToChTypeName chType) ':<>: 'Text ")) is unsupported"
    :$$: 'Text "Use LowCardinality(Nullable(" ':<>: 'Text (ToChTypeName chType) ':<>: 'Text ")) instead"
    )
  ToLowCardinalitySupported (Nullable a) = Nullable (ToLowCardinalitySupported a)
  ToLowCardinalitySupported ChString = ChString
  ToLowCardinalitySupported ChInt32 = ChInt32
  ToLowCardinalitySupported ChInt64 = ChInt64
  ToLowCardinalitySupported ChInt128 = ChInt128
  ToLowCardinalitySupported ChDateTime = ChDateTime
  ToLowCardinalitySupported chType = TypeError
    (    'Text "LowCardinality("  ':<>: 'Text (ToChTypeName chType)  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"    ':$$: 'Text "  ChInt32"
    ':$$: 'Text "  ChInt64"     ':$$: 'Text "  ChInt128"
    ':$$: 'Text "  ChDateTime"
    )

instance
  ( Serializable (ToLowCardinalitySupported chType)
  ) => Serializable (LowCardinality chType) where
  serialize (LowCardinality value) = serialize value

instance
  ( Deserializable (ToLowCardinalitySupported chType)
  ) => Deserializable (LowCardinality chType) where
    deserialize = LowCardinality . deserialize

instance {-# OVERLAPPING #-}
  ( ToChType (ToLowCardinalitySupported chType) (ToLowCardinalitySupported inputType)
  ) => ToChType (LowCardinality chType) (LowCardinality inputType) where
  toChType (LowCardinality value) = LowCardinality $ toChType value

instance {-# OVERLAPPING #-}
  ( ToChType (ToLowCardinalitySupported chType) inputType
  ) => ToChType (LowCardinality chType) inputType where
  toChType value = LowCardinality $ toChType value


instance {-# OVERLAPPING #-}
  ( FromChType (ToLowCardinalitySupported chType) (ToLowCardinalitySupported outputType)
  ) => FromChType (LowCardinality chType) (LowCardinality outputType) where
  fromChType (LowCardinality value) = LowCardinality $ fromChType value


instance {-# OVERLAPPING #-}
  ( FromChType (ToLowCardinalitySupported chType) outputType
  ) => FromChType (LowCardinality chType) outputType where
  fromChType (LowCardinality value) = fromChType value




-- | ClickHouse UUID column type
newtype                 ChUUID = ChUUID UUID   deriving newtype (Show, Eq)
instance IsChType       ChUUID        where type ToChTypeName ChUUID = "UUID"
instance Serializable   ChUUID        where serialize (ChUUID uuid)   = UUID.toASCIIBytes uuid
instance Deserializable ChUUID        where deserialize bs = ChUUID $ fromJust $ UUID.fromASCIIBytes bs
instance ToChType       ChUUID ChUUID where toChType = id
instance ToChType       ChUUID UUID   where toChType = ChUUID
instance FromChType     ChUUID ChUUID where fromChType = id
instance FromChType     ChUUID UUID   where fromChType (ChUUID uuid) = uuid

nilChUUID :: ChUUID
nilChUUID = toChType UUID.nil




-- | ClickHouse String column type
newtype ChString = ChString ByteString  deriving newtype (Show, Eq, IsString)
instance IsChType       ChString              where type ToChTypeName ChString = "String"
instance Serializable   ChString              where serialize = coerce
instance Deserializable ChString              where deserialize = ChString
instance ToChType       ChString   ChString   where toChType = id
instance ToChType       ChString   ByteString where toChType = ChString . escape
instance ToChType       ChString   String     where toChType = ChString . escape . BS8.pack
instance ToChType       ChString   Text       where toChType = ChString . escape . Text.encodeUtf8
instance ToChType       ChString   Int        where toChType = ChString . escape . BS8.pack . show
instance FromChType     ChString   ChString   where fromChType = id
instance FromChType     ChString ByteString   where fromChType (ChString bs) = bs

escape :: ByteString -> ByteString
escape = BS8.concatMap (\sym -> if sym == '\t' then "\\t" else if sym == '\n' then "\\n" else BS8.singleton sym)




-- | ClickHouse Int8 column type
newtype                 ChInt8 = ChInt8 Int8  deriving newtype (Show, Eq)
instance IsChType       ChInt8        where type ToChTypeName ChInt8 = "Int8"
instance Serializable   ChInt8        where serialize = BS8.pack . show @ChInt8 . coerce
instance Deserializable ChInt8        where deserialize = ChInt8 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType       ChInt8 ChInt8 where toChType = id
instance ToChType       ChInt8 Int8   where toChType = ChInt8
instance FromChType     ChInt8 ChInt8 where fromChType = id
instance FromChType     ChInt8 Int8   where fromChType (ChInt8 int8) = int8




-- | ClickHouse Int16 column type
newtype                 ChInt16 = ChInt16 Int16  deriving newtype (Show, Eq)
instance IsChType       ChInt16         where type ToChTypeName ChInt16 = "Int16"
instance Serializable   ChInt16         where serialize = BS8.pack . show @Int16 . coerce
instance Deserializable ChInt16         where deserialize  = ChInt16 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType       ChInt16 ChInt16 where toChType = id
instance ToChType       ChInt16 Int16   where toChType = ChInt16
instance FromChType     ChInt16 ChInt16 where fromChType = id
instance FromChType     ChInt16 Int16   where fromChType (ChInt16 int16) = int16




-- | ClickHouse Int32 column type
newtype                 ChInt32 = ChInt32 Int32  deriving newtype (Show, Eq)
instance IsChType       ChInt32         where type ToChTypeName ChInt32 = "Int32"
instance Serializable   ChInt32         where serialize = BS8.pack . show @ChInt32 . coerce
instance Deserializable ChInt32         where deserialize = ChInt32 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType       ChInt32 ChInt32 where toChType = id
instance ToChType       ChInt32 Int32   where toChType = ChInt32
instance FromChType     ChInt32 ChInt32 where fromChType = id
instance FromChType     ChInt32 Int32   where fromChType (ChInt32 int32) = int32




-- | ClickHouse Int64 column type
newtype                 ChInt64 = ChInt64 Int64  deriving newtype (Show, Eq)
instance IsChType       ChInt64         where type ToChTypeName ChInt64 = "Int64"
instance Serializable   ChInt64         where serialize (ChInt64 val)    = BS8.pack $ show val
instance Deserializable ChInt64         where deserialize = ChInt64 . fromInteger . fst . fromJust . BS8.readInteger
instance ToChType       ChInt64 ChInt64 where toChType = id
instance ToChType       ChInt64 Int64   where toChType = ChInt64 . fromIntegral
instance ToChType       ChInt64 Int     where toChType = ChInt64 . fromIntegral
instance FromChType     ChInt64 ChInt64 where fromChType = id
instance FromChType     ChInt64 Int64   where fromChType = coerce




-- | ClickHouse Int128 column type
newtype                    ChInt128 = ChInt128   Int128  deriving newtype (Show, Eq)
instance IsChType ChInt128 where
  type ToChTypeName ChInt128 = "Int128"
instance Serializable   ChInt128          where serialize = BS8.pack . show @ChInt128 . coerce
instance Deserializable ChInt128          where deserialize = ChInt128 . fromInteger . fst . fromJust . BS8.readInteger
instance ToChType       ChInt128 ChInt128 where toChType = id
instance ToChType       ChInt128 Int128   where toChType = ChInt128 . fromIntegral
instance FromChType     ChInt128 ChInt128 where fromChType = id
instance FromChType     ChInt128 Int128   where fromChType (ChInt128 int128) = int128




-- | ClickHouse UInt8 column type
newtype                 ChUInt8 = ChUInt8 Word8  deriving newtype (Show, Eq)
instance IsChType       ChUInt8         where type ToChTypeName ChUInt8 = "UInt8"
instance Serializable   ChUInt8         where serialize = BS8.pack . show @ChUInt8 . coerce
instance Deserializable ChUInt8         where deserialize = ChUInt8 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType       ChUInt8 ChUInt8 where toChType = id
instance ToChType       ChUInt8 Word8   where toChType = ChUInt8
instance FromChType     ChUInt8 ChUInt8 where fromChType = id
instance FromChType     ChUInt8 Word8   where fromChType (ChUInt8 word8) = word8




-- | ClickHouse UInt16 column type
newtype                 ChUInt16 = ChUInt16 Word16  deriving newtype (Show, Eq)
instance IsChType       ChUInt16          where type ToChTypeName ChUInt16 = "UInt16"
instance Serializable   ChUInt16          where serialize = BS8.pack . show @ChUInt16 . coerce
instance Deserializable ChUInt16          where deserialize = ChUInt16 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType       ChUInt16 ChUInt16 where toChType = id
instance ToChType       ChUInt16 Word16   where toChType = ChUInt16
instance FromChType     ChUInt16 ChUInt16 where fromChType = id
instance FromChType     ChUInt16 Word16   where fromChType (ChUInt16 word16) = word16




-- | ClickHouse UInt32 column type
newtype                  ChUInt32 = ChUInt32 Word32  deriving newtype (Show, Eq)
instance IsChType        ChUInt32          where type ToChTypeName ChUInt32 = "UInt32"
instance Serializable    ChUInt32          where serialize (ChUInt32 val)    = BS8.pack $ show val
instance Deserializable  ChUInt32          where deserialize = ChUInt32 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType        ChUInt32 ChUInt32 where toChType = id
instance ToChType        ChUInt32 Word32   where toChType = ChUInt32 . fromIntegral
instance FromChType      ChUInt32 ChUInt32 where fromChType = id
instance FromChType      ChUInt32 Word32   where fromChType (ChUInt32 w32) = w32




-- | ClickHouse UInt64 column type
newtype                 ChUInt64 = ChUInt64 Word64  deriving newtype (Show, Eq)
instance IsChType       ChUInt64          where type ToChTypeName ChUInt64 = "UInt64"
instance Serializable   ChUInt64          where serialize = BS8.pack . show @ChUInt64 . coerce
instance Deserializable ChUInt64          where deserialize = ChUInt64 . fromIntegral . fst . fromJust . BS8.readInteger
instance ToChType       ChUInt64 ChUInt64 where toChType = id
instance ToChType       ChUInt64 Word64   where toChType = ChUInt64 . fromIntegral
instance FromChType     ChUInt64 ChUInt64 where fromChType = id
instance FromChType     ChUInt64 Word64   where fromChType (ChUInt64 w64) = w64




-- | ClickHouse DateTime column type
newtype                 ChDateTime = ChDateTime Word32  deriving newtype (Show, Eq)
instance IsChType       ChDateTime            where type ToChTypeName ChDateTime = "DateTime"
instance Serializable   ChDateTime            where serialize (ChDateTime w32) = let time = BS8.pack $ show w32 in BS8.replicate (10 - BS8.length time) '0' <>  time
instance Deserializable ChDateTime            where
  deserialize
    = ChDateTime . fromInteger
    . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
    . fromJust . parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    . BS8.unpack
instance ToChType       ChDateTime ChDateTime where toChType = id
instance ToChType       ChDateTime Word32     where toChType = ChDateTime
instance ToChType       ChDateTime UTCTime    where toChType = ChDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType       ChDateTime ZonedTime  where toChType = ChDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC
instance FromChType     ChDateTime ChDateTime where fromChType = id
instance FromChType     ChDateTime Word32     where fromChType (ChDateTime word32) = word32

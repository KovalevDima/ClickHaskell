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
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module ClickHaskell.TableDsl.DbTypes
  ( IsChType(..), ToChType(toChType), ToChTypeName
  , fromChType

  , ChDateTime

  , ChInt8
  , ChInt16
  , ChInt32
  , ChInt64
  , ChInt128

  , ChUInt8
  , ChUInt16
  , ChUInt32
  , ChUInt64

  , ChString
  , ChUUID, nilChUUID

  , Nullable
  , LowCardinality
  ) where

-- External dependencies
import Data.UUID     as UUID (UUID, fromASCIIBytes, toASCIIBytes, nil)
import Data.WideWord (Int128)

-- GHC included libraries imports
import GHC.TypeLits          (AppendSymbol, ErrorMessage (..), KnownSymbol, Symbol, TypeError, symbolVal)
import Data.ByteString       as BS (ByteString)
import Data.ByteString.Char8 as BS8 (concatMap, pack, readInt, readInteger, singleton, unpack, replicate, length)
import Data.Maybe            (fromJust)
import Data.Int              (Int32, Int16, Int8, Int64)
import Data.Kind             (Type)
import Data.Proxy            (Proxy (Proxy))
import Data.Text             as Text (Text, pack)
import Data.Text.Encoding    as Text (encodeUtf8)
import Data.Time             (UTCTime, defaultTimeLocale, nominalDiffTimeToSeconds, parseTimeM, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.String           (IsString)
import Data.Word             (Word64, Word32, Word16, Word8)


type family (ToChTypeName columnType) :: Symbol
newtype ChTypeName = ChTypeName Text deriving newtype (Show, Semigroup, IsString)

-- | ClickHouse type-level convertion class
class KnownSymbol (ToChTypeName chType)
  => IsChType chType where

  -- | Get original ClickHouse type name
  --
  -- >>> originalName $ Proxy @ChInt32
  -- "Int32"
  originalName :: Proxy chType -> Text
  originalName _ = Text.pack . symbolVal $ (Proxy :: Proxy (ToChTypeName chType))

  render       :: chType -> BS.ByteString
  parse        :: BS.ByteString -> chType

-- | ClickHouse type requirements typeclass
class IsChType chType
  => ToChType chType     inputType where
  toChType :: inputType -> chType

class IsChType chType
  => FromChType chType outputType | chType -> outputType where
  fromChType :: chType -> outputType

-- | ClickHouse Nullable(T) column type (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
type instance ToChTypeName (Maybe chType) = NullableTypeName chType

instance (KnownSymbol (NullableTypeName chType), IsChType chType)
  =>     IsChType (Maybe chType)
  where
  render = renderNullable
  parse "\\N"      = Nothing
  parse someTypeBs = Just (parse someTypeBs) :: Maybe chType

renderNullable :: IsChType chType => Maybe chType -> ByteString
renderNullable Nothing           = "\\N"
renderNullable (Just val)        = render val

instance (KnownSymbol (NullableTypeName chType), IsChType chType, ToChType chType inputType)
  =>     ToChType (Maybe chType) (Maybe inputType) where
    toChType Nothing  = Nothing
    toChType (Just a) = Just (toChType @chType a)


-- | ClickHouse LowCardinality(T) column type
type PermittedType :: Type -> Type
type family PermittedType a where
  PermittedType (Nullable a) = Nullable (PermittedType a)
  PermittedType ChString = ChString
  PermittedType ChInt32 = ChInt32
  PermittedType ChInt64 = ChInt64
  PermittedType ChInt128 = ChInt128
  PermittedType ChDateTime = ChDateTime
  PermittedType chType = TypeError 
    (    'Text "LowCardinality("  ':<>: 'Text (ToChTypeName chType)  ':<>: 'Text ") is unsupported"
    ':$$: 'Text "Use one of these types:"
    ':$$: 'Text "  ChString"    ':$$: 'Text "  ChInt32"
    ':$$: 'Text "  ChInt64"     ':$$: 'Text "  ChInt128"
    ':$$: 'Text "  ChDateTime"
    )

newtype LowCardinality chType = LowCardinality (PermittedType chType)
instance Eq (PermittedType chType) => Eq (LowCardinality chType) where
  (==) (LowCardinality lc1) (LowCardinality lc2) = lc1 == lc2 
instance Show (PermittedType a) => Show (LowCardinality a) where
  show (LowCardinality a) = show a

type instance ToChTypeName (LowCardinality chType) =
  "LowCardinality(" `AppendSymbol` ToChTypeName (PermittedType chType) `AppendSymbol` ")"

instance 
  ( IsChType (LowCardinality chType)
  , ToChType (PermittedType chType) inputType) => 
  ToChType (LowCardinality chType) inputType where
  toChType value = LowCardinality $ toChType value
instance {-# OVERLAPPING #-}
  ( TypeError
    (    'Text "Nullable(LowCardinality("     ':<>: 'Text (ToChTypeName chType) ':<>: 'Text ")) is unsupported"
    :$$: 'Text "Use LowCardinality(Nullable(" ':<>: 'Text (ToChTypeName chType) ':<>: 'Text ")) instead"
    )
  , IsChType chType, KnownSymbol (ToChTypeName (Nullable (LowCardinality chType))))
  => (IsChType (Nullable (LowCardinality chType)))


instance 
  ( KnownSymbol (ToChTypeName (LowCardinality chType))
  , IsChType chType
  , IsChType (PermittedType chType)) =>
  IsChType (LowCardinality chType) where
  render (LowCardinality value) = render value
  parse value = LowCardinality $ parse value


-- | ClickHouse UUID column type
newtype                    ChUUID = ChUUID      UUID   deriving newtype (Show, Eq)
type instance ToChTypeName ChUUID = "UUID"
instance      IsChType     ChUUID      where
  render (ChUUID uuid)   = UUID.toASCIIBytes uuid
  parse bs = ChUUID $ fromJust $ UUID.fromASCIIBytes bs
instance      ToChType     ChUUID UUID where toChType = ChUUID
instance FromChType ChUUID UUID where fromChType (ChUUID uuid) = uuid

nilChUUID :: UUID
nilChUUID = UUID.nil


-- | ClickHouse String column type
newtype ChString                    = ChString  ByteString   deriving newtype (Show, Eq, IsString)
type instance ToChTypeName ChString = "String"
instance      IsChType     ChString        where
  render (ChString val) = val
  parse = ChString
instance      ToChType     ChString String where toChType = ChString . escape . BS8.pack
instance      ToChType     ChString Text   where toChType = ChString . escape . Text.encodeUtf8
instance      ToChType     ChString Int    where toChType = ChString . escape . BS8.pack . show
instance FromChType ChString ByteString where fromChType (ChString bs) = bs

escape :: ByteString -> ByteString
escape = BS8.concatMap (\sym -> if sym == '\t' then "\\t" else if sym == '\n' then "\\n" else BS8.singleton sym)


-- | ClickHouse Int8 column type
newtype                    ChInt8 = ChInt8       Int8    deriving newtype (Show, Eq)
type instance ToChTypeName ChInt8 = "Int8"
instance      IsChType     ChInt8       where
  render (ChInt8 val)   = BS8.pack $ show val
  parse = ChInt8 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType   ChInt8 Int8 where toChType = ChInt8
instance FromChType ChInt8 Int8 where fromChType (ChInt8 int8) = int8

-- | ClickHouse Int16 column type
newtype                    ChInt16 = ChInt16     Int16   deriving newtype (Show, Eq)
type instance ToChTypeName ChInt16 = "Int16"
instance      IsChType     ChInt16       where
  render (ChInt16 val)   = BS8.pack $ show val
  parse = ChInt16 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType   ChInt16 Int16 where toChType = ChInt16
instance FromChType ChInt16 Int16 where fromChType (ChInt16 int16) = int16


-- | ClickHouse Int32 column type
newtype                    ChInt32 = ChInt32     Int32   deriving newtype (Show, Eq)
type instance ToChTypeName ChInt32 = "Int32"
instance      IsChType     ChInt32       where
  render (ChInt32 val)   = BS8.pack $ show val
  parse = ChInt32 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType   ChInt32 Int32 where toChType = ChInt32
instance FromChType ChInt32 Int32 where fromChType (ChInt32 int32) = int32


-- | ClickHouse Int64 column type
newtype                    ChInt64 = ChInt64     Int64   deriving newtype (Show, Eq)
type instance ToChTypeName ChInt64 = "Int64"
instance      IsChType     ChInt64   where
  render (ChInt64 val)    = BS8.pack $ show val
  parse                   = ChInt64 . fromInteger . fst . fromJust . BS8.readInteger
instance Integral a
  =>          ToChType     ChInt64 a where toChType = ChInt64 . fromIntegral
instance FromChType ChInt64 Int64 where fromChType (ChInt64 int64) = int64


-- | ClickHouse Int128 column type
newtype                    ChInt128 = ChInt128   Int128  deriving newtype (Show, Eq)
type instance ToChTypeName ChInt128 = "Int128"
instance      IsChType     ChInt128   where
  render (ChInt128 val)   = BS8.pack $ show val
  parse                   = ChInt128 . fromInteger . fst . fromJust . BS8.readInteger
instance Integral a
  =>          ToChType     ChInt128 a where toChType = ChInt128 . fromIntegral
instance FromChType ChInt128 Int128 where fromChType (ChInt128 int128) = int128


-- | ClickHouse UInt8 column type
newtype                    ChUInt8 = ChUInt8     Word8   deriving newtype (Show, Eq)
type instance ToChTypeName ChUInt8 = "UInt8"
instance      IsChType     ChUInt8       where
  render (ChUInt8 val)   = BS8.pack $ show val
  parse = ChUInt8 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType   ChUInt8 Word8 where toChType = ChUInt8
instance FromChType ChUInt8 Word8 where fromChType (ChUInt8 word8) = word8

-- | ClickHouse UInt16 column type
newtype                    ChUInt16 = ChUInt16   Word16  deriving newtype (Show, Eq)
type instance ToChTypeName ChUInt16 = "UInt16"
instance      IsChType     ChUInt16       where
  render (ChUInt16 val)   = BS8.pack $ show val
  parse = ChUInt16 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType   ChUInt16 Word16 where toChType = ChUInt16
instance FromChType ChUInt16 Word16 where fromChType (ChUInt16 word16) = word16


-- | ClickHouse UInt32 column type
newtype                    ChUInt32 = ChUInt32    Word32   deriving newtype (Show)
type instance ToChTypeName ChUInt32 = "UInt32"
instance      IsChType     ChUInt32   where
  render (ChUInt32 val)    = BS8.pack $ show val
  parse                   = ChUInt32 . fromIntegral . fst . fromJust . BS8.readInt
instance ToChType   ChUInt32 Word32 where toChType = ChUInt32 . fromIntegral
instance FromChType ChUInt32 Word32 where fromChType (ChUInt32 w32) = w32


-- | ClickHouse UInt64 column type
newtype                    ChUInt64 = ChUInt64    Word64   deriving newtype (Show)
type instance ToChTypeName ChUInt64 = "UInt64"
instance      IsChType     ChUInt64   where
  render (ChUInt64 val)    = BS8.pack $ show val
  parse                   = ChUInt64 . fromIntegral . fst . fromJust . BS8.readInteger
instance Integral a
  =>          ToChType     ChUInt64 a where toChType = ChUInt64 . fromIntegral
instance FromChType ChUInt64 Word64 where fromChType (ChUInt64 w64) = w64


-- | ClickHouse DateTime column type
newtype                    ChDateTime =  ChDateTime Word32  deriving newtype (Show, Eq)
type instance ToChTypeName ChDateTime = "DateTime"
instance      IsChType     ChDateTime         where
  render (ChDateTime w32) = let time = BS8.pack $ show w32 in BS8.replicate (10 - BS8.length time) '0' <>  time
  parse = ChDateTime . fromInteger . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . fromJust . parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" . BS8.unpack
instance      ToChType     ChDateTime Word32  where toChType = ChDateTime
instance      ToChType     ChDateTime UTCTime where toChType = ChDateTime . floor . utcTimeToPOSIXSeconds
instance      ToChType     ChDateTime ZonedTime where toChType = ChDateTime . floor . utcTimeToPOSIXSeconds . zonedTimeToUTC
instance FromChType ChDateTime Word32 where fromChType (ChDateTime word32) = word32

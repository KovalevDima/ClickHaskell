{-# LANGUAGE
    ConstraintKinds
  , DuplicateRecordFields
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , TupleSections
  , OverloadedStrings
  , RecordWildCards
  , NoFieldSelectors
#-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}

module ClickHaskell.NativeProtocol where

-- Internal dependencies
import ClickHaskell.DbTypes
import Paths_ClickHaskell (version)

-- GHC included
import Data.Text (Text)
import Data.Version (Version (..))
import Data.String (IsString(..))
import Control.Monad (forM, replicateM)
import Data.Binary.Get
import Data.Binary.Get.Internal (readN)
import Data.Binary.Put
import Data.Bits
import Data.ByteString as BS (length, take)
import Data.ByteString.Builder (Builder, stringUtf8, word8, byteString)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Coerce (coerce)
import Data.Kind (Type, Constraint)
import Data.Typeable (Proxy (..))
import Debug.Trace (traceShowId)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)
import GHC.Word (Word64)

-- * Compatibility

latestSupportedRevision :: ProtocolRevision
latestSupportedRevision = mostRecentRevision

-- * Client packets

data ClientPacketType
  = Hello
  | Query
  | Data
  | Cancel
  | Ping
  | TablesStatusRequest
  | KeepAlive
  | Scalar
  | IgnoredPartUUIDs
  | ReadTaskResponse
  | MergeTreeReadTaskResponse
  | SSHChallengeRequest
  | SSHChallengeResponse
  deriving (Enum, Show)

type family PacketTypeNumber (packetType :: ClientPacketType)
  where
  PacketTypeNumber Hello = 0
  PacketTypeNumber Query = 1
  PacketTypeNumber Data = 2
  PacketTypeNumber Cancel = 3
  PacketTypeNumber Ping = 4
  PacketTypeNumber TablesStatusRequest = 5
  PacketTypeNumber KeepAlive = 6
  PacketTypeNumber Scalar = 7
  PacketTypeNumber IgnoredPartUUIDs = 8
  PacketTypeNumber ReadTaskResponse = 9
  PacketTypeNumber MergeTreeReadTaskResponse = 10
  PacketTypeNumber SSHChallengeRequest = 11
  PacketTypeNumber SSHChallengeResponse = 12

data Packet (packetType :: ClientPacketType) = MkPacket
instance KnownNat (PacketTypeNumber packetType) => Show (Packet (packetType :: ClientPacketType)) where
  show _ = show . toEnum @ClientPacketType . fromIntegral $ packetNumVal @packetType

packetNumVal :: forall packetType . KnownNat (PacketTypeNumber packetType) => UVarInt
packetNumVal = fromIntegral . natVal $ Proxy @(PacketTypeNumber packetType)

instance
  KnownNat (PacketTypeNumber packetType)
  =>
  Serializable (Packet (packetType :: ClientPacketType)) where
  serialize rev _ = serialize @UVarInt rev (packetNumVal @packetType)

instance Deserializable (Packet (packetType :: ClientPacketType)) where
  deserialize _rev = pure (MkPacket @packetType)


-- ** Hello

data HelloParameters = MkHelloParameters
  { chDatabase :: Text
  , chLogin :: Text
  , chPass :: Text
  }

mkHelloPacket :: HelloParameters -> HelloPacket
mkHelloPacket MkHelloParameters{chDatabase, chLogin, chPass} =
  MkHelloPacket
    { packet_type          = MkPacket
    , client_name, client_version_major, client_version_minor
    , tcp_protocol_version = latestSupportedRevision
    , default_database     = toChType chDatabase
    , user                 = toChType chLogin
    , password             = toChType chPass
    }

data HelloPacket = MkHelloPacket
  { packet_type          :: Packet Hello
  , client_name          :: ChString
  , client_version_major :: UVarInt
  , client_version_minor :: UVarInt
  , tcp_protocol_version :: ProtocolRevision
  , default_database     :: ChString
  , user                 :: ChString
  , password             :: ChString
  }
  deriving (Generic, Serializable)


mkAddendum :: Addendum
mkAddendum = MkAddendum
  { quota_key = MkSinceRevision ""
  }

data Addendum = MkAddendum
  { quota_key :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY
  }
  deriving (Generic, Serializable)


-- ** Ping

mkPingPacket :: PingPacket
mkPingPacket = MkPingPacket{packet_type = MkPacket}

data PingPacket = MkPingPacket{packet_type :: Packet Ping}
  deriving (Generic, Serializable)


-- ** Query

mkQueryPacket :: ProtocolRevision -> ChString -> ChString -> QueryPacket
mkQueryPacket chosenRev user query = MkQueryPacket
  { query_packet = MkPacket
  , query_id = ""
  , client_info                    = MkSinceRevision MkClientInfo
    { query_kind                   = InitialQuery
    , initial_user                 = user
    , initial_query_id             = ""
    , initial_adress               = "0.0.0.0:0"
    , initial_time                 = MkSinceRevision 0
    , interface_type               = 1 -- [tcp - 1, http - 2]
    , os_user                      = "dmitry"
    , hostname                     = "desktop"
    , client_name, client_version_major, client_version_minor
    , client_revision              = chosenRev
    , quota_key                    = MkSinceRevision ""
    , distrubuted_depth            = MkSinceRevision 0
    , client_version_patch
    , open_telemetry               = MkSinceRevision 0
    , collaborate_with_initiator   = MkSinceRevision 0
    , count_participating_replicas = MkSinceRevision 0
    , number_of_current_replica    = MkSinceRevision 0
    }
  , settings           = MkDbSettings
  , interserver_secret = MkSinceRevision ""
  , query_stage        = Complete
  , compression        = 0
  , query              = query
  , parameters         = MkSinceRevision MkQueryParameters
  }

data QueryPacket = MkQueryPacket
  { query_packet       :: Packet Query
  , query_id           :: ChString
  , client_info        :: ClientInfo `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_INFO
  , settings           :: DbSettings
  , interserver_secret :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
  , query_stage        :: QueryStage
  , compression        :: UVarInt
  , query              :: ChString
  , parameters         :: QueryParameters `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
  }
  deriving (Generic, Serializable)

data DbSettings = MkDbSettings
instance Serializable DbSettings where serialize rev _ = serialize @ChString rev ""

data QueryParameters = MkQueryParameters
instance Serializable QueryParameters where serialize rev _ = serialize @ChString rev ""

data QueryStage
  = FetchColumns
  | WithMergeableState
  | Complete
  | WithMergeableStateAfterAggregation
  | WithMergeableStateAfterAggregationAndLimit
  deriving (Enum)

instance Serializable QueryStage where
  serialize rev = serialize @UVarInt rev . fromIntegral . fromEnum

queryStageCode :: QueryStage -> UVarInt
queryStageCode = fromIntegral . fromEnum

data Flags = IMPORTANT | CUSTOM | OBSOLETE
flagCode :: Flags -> ChUInt64
flagCode IMPORTANT = 0x01
flagCode CUSTOM    = 0x02
flagCode OBSOLETE  = 0x04

data ClientInfo = MkClientInfo
  { query_kind                   :: QueryKind
  , initial_user                 :: ChString
  , initial_query_id             :: ChString
  , initial_adress               :: ChString
  , initial_time                 :: ChInt64 `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
  , interface_type               :: ChUInt8
  , os_user                      :: ChString
  , hostname                     :: ChString
  , client_name                  :: ChString
  , client_version_major         :: UVarInt
  , client_version_minor         :: UVarInt
  , client_revision              :: ProtocolRevision
  , quota_key                    :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
  , distrubuted_depth            :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
  , client_version_patch         :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , open_telemetry               :: ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_OPENTELEMETRY
  , collaborate_with_initiator   :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , count_participating_replicas :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , number_of_current_replica    :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  }
  deriving (Generic, Serializable)

data QueryKind = NoQuery | InitialQuery | SecondaryQuery
  deriving (Enum)

instance Serializable QueryKind where
  serialize rev = serialize @ChUInt8 rev . fromIntegral . fromEnum


-- ** Data

mkDataPacket :: ChString -> UVarInt -> UVarInt -> DataPacket
mkDataPacket table_name columns_count rows_count =
  MkDataPacket
    { packet_type   = MkPacket
    , table_name
    , block_info    = MkBlockInfo
      { field_num1   = 1, is_overflows = 0
      , field_num2   = 2, bucket_num   = -1
      , eof          = 0
      }
    , columns_count
    , rows_count
    }

data DataPacket = MkDataPacket
  { packet_type   :: Packet Data
  , table_name    :: ChString
  , block_info    :: BlockInfo
  , columns_count :: UVarInt
  , rows_count    :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable, Show)

data BlockInfo = MkBlockInfo
  { field_num1   :: UVarInt, is_overflows :: ChUInt8
  , field_num2   :: UVarInt, bucket_num   :: ChInt32
  , eof          :: UVarInt
  }
  deriving (Generic, Serializable, Deserializable, Show)




-- * Server packets

data ServerPacketType where
  HelloResponse :: HelloResponse -> ServerPacketType
  DataResponse :: DataPacket -> ServerPacketType
  Exception :: ExceptionPacket -> ServerPacketType
  Progress :: ProgressPacket -> ServerPacketType
  Pong :: ServerPacketType
  EndOfStream :: ServerPacketType
  ProfileInfo :: ProfileInfo -> ServerPacketType
  Totals :: ServerPacketType
  Extremes :: ServerPacketType
  TablesStatusResponse :: ServerPacketType
  Log :: ServerPacketType
  TableColumns :: TableColumns -> ServerPacketType
  UUIDs :: ServerPacketType
  ReadTaskRequest :: ServerPacketType
  ProfileEvents :: ServerPacketType
  UnknownPacket :: UVarInt -> ServerPacketType

instance Deserializable ServerPacketType where
  deserialize rev = do
    packetNum <- deserialize @UVarInt rev
    case packetNum of
      0  -> HelloResponse <$> deserialize rev
      1  -> DataResponse <$> deserialize rev
      2  -> Exception <$> deserialize rev
      3  -> Progress <$> deserialize rev
      4  -> pure Pong
      5  -> pure EndOfStream
      6  -> ProfileInfo <$> deserialize rev
      7  -> pure Totals
      8  -> pure Extremes
      9  -> pure TablesStatusResponse
      10 -> pure Log
      11 -> TableColumns <$> deserialize rev
      12 -> pure UUIDs
      13 -> pure ReadTaskRequest
      14 -> pure ProfileEvents
      _  -> pure $ UnknownPacket packetNum

instance Show ServerPacketType where
  show (HelloResponse hello) = "HelloResponse " <> show hello
  show (DataResponse dataPacket) = "DataResponse " <> show dataPacket
  show (Exception exception) = "Exception " <> show exception
  show (Progress progress) = "Progress " <> show progress
  show Pong = "Pong"
  show EndOfStream = "EndOfStream"
  show (ProfileInfo profileInfo) = "ProfileInfo " <> show profileInfo
  show Totals = "Totals"
  show Extremes = "Extremes"
  show TablesStatusResponse = "TablesStatusResponse"
  show Log = "Log"
  show (TableColumns tabelColumnsPacket) = "TableColumns " <> show tabelColumnsPacket
  show UUIDs = "UUIDs"
  show ReadTaskRequest = "ReadTaskRequest"
  show ProfileEvents = "ProfileEvents"
  show (UnknownPacket packetNum) = "UnknownPacket: " <> show packetNum

-- ** HelloResponse

{-
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Client/Connection.cpp#L520
-}
data HelloResponse = MkHelloResponse
  { server_name                    :: ChString
  , server_version_major           :: UVarInt
  , server_version_minor           :: UVarInt
  , server_revision                :: ProtocolRevision
  , server_parallel_replicas_proto :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL
  , server_timezone                :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE
  , server_display_name            :: ChString `SinceRevision` DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME
  , server_version_patch           :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , proto_send_chunked_srv         :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , proto_recv_chunked_srv         :: ChString `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , password_complexity_rules      :: [PasswordComplexityRules] `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES
  , read_nonce                     :: ChUInt64 `SinceRevision` DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2
  }
  deriving (Generic, Show)

instance Deserializable HelloResponse where
  deserialize revision = do
    server_name                    <- deserialize revision
    server_version_major           <- deserialize revision
    server_version_minor           <- deserialize revision
    server_revision                <- deserialize revision
    -- Override current protocol revision for backward compatibility
    let chosenRevision = min server_revision revision
    server_parallel_replicas_proto <- deserialize chosenRevision
    server_timezone                <- deserialize chosenRevision
    server_display_name            <- deserialize chosenRevision
    server_version_patch           <- deserialize chosenRevision
    proto_send_chunked_srv         <- deserialize chosenRevision
    proto_recv_chunked_srv         <- deserialize chosenRevision
    password_complexity_rules      <- deserialize chosenRevision
    read_nonce                     <- deserialize chosenRevision
    pure MkHelloResponse{..}

data PasswordComplexityRules = MkPasswordComplexityRules
  { original_pattern  :: ChString
  , exception_message :: ChString
  }
  deriving (Generic, Deserializable, Show)

instance Deserializable [PasswordComplexityRules] where
  deserialize rev = do
    len <- deserialize @UVarInt rev
    replicateM (fromIntegral len) (deserialize @PasswordComplexityRules rev)

-- ** Exception

data ExceptionPacket = MkExceptionPacket
  { code        :: ChInt32
  , name        :: ChString
  , message     :: ChString
  , stack_trace :: ChString
  , nested      :: ChUInt8
  }
  deriving (Generic, Deserializable, Show)

-- ** Progress

data ProgressPacket = MkProgressPacket
  { rows        :: UVarInt
  , bytes       :: UVarInt
  , total_rows  :: UVarInt
  , total_bytes :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_rows  :: UVarInt `SinceRevision` DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , wrote_bytes :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  , elapsed_ns  :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  }
  deriving (Generic, Deserializable, Show)

-- ** ProfileInfo

data ProfileInfo = MkProfileInfo
  { rows                         :: UVarInt
  , blocks                       :: UVarInt
  , bytes                        :: UVarInt
  , applied_limit                :: ChUInt8
  , rows_before_limit            :: UVarInt
  , calculated_rows_before_limit :: ChUInt8
  , applied_aggregation          :: ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  , rows_before_aggregation      :: UVarInt `SinceRevision` DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  }
  deriving (Generic, Deserializable, Show)

-- ** TableColumns

data TableColumns = MkTableColumns
  { table_name :: ChString
  , table_columns :: ChString
  }
  deriving (Generic, Deserializable, Show)








-- * Deserialization

-- ** Generic API

type GenericReadable record hasColumns =
  ( Generic record
  , GReadable (GetColumns hasColumns) (Rep record)
  )

class
  ( HasColumns hasColumns
  , DeserializableColumns (Columns (GetColumns hasColumns))
  ) =>
  ReadableFrom hasColumns record
  where
  default deserializeColumns :: GenericReadable record hasColumns => ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns :: ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns rev size = do
    list <- gFromColumns @(GetColumns hasColumns) rev size
    pure $ do
      element <- list
      case to element of res -> pure $! res

  default readingColumns :: GenericReadable record hasColumns => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)


class GReadable (columns :: [Type]) f
  where
  gFromColumns :: ProtocolRevision -> UVarInt -> Get [f p]
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . M1) <$> gFromColumns @columns rev size
  gReadingColumns = gReadingColumns @columns @f

instance
  GReadable columns (left :*: (right1 :*: right2))
  =>
  GReadable columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = do
    list <- gFromColumns @columns rev size
    pure [(l :*: r1) :*: r2 | (l :*: (r1 :*: r2)) <- list]
  gReadingColumns = gReadingColumns @columns @(left :*: (right1 :*: right2))


instance
  ( KnownColumn (Column name chType)
  , GReadable '[Column name chType] (S1 (MetaSel (Just name) a b f) rec)
  , GReadable restColumns right
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  )
  =>
  GReadable columns (S1 (MetaSel (Just name) a b f) rec :*: right)
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = do
    zipWith (:*:)
      <$> gFromColumns @'[Column name chType] rev size
      <*> gFromColumns @restColumns rev size
  gReadingColumns =
    renderColumnName @(Column name chType)
    <> ", " <> gReadingColumns @restColumns @right

instance
  ( KnownColumn (Column name chType)
  , DeserializableColumn (Column name chType)
  , FromChType chType inputType
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) => GReadable columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gFromColumns #-}
  gFromColumns rev size = map (M1 . K1 . fromChType @chType) . columnValues <$> deserializeColumn @(Column name chType) rev size
  gReadingColumns = renderColumnName @(Column name chType)


-- ** Raw columns deserialization

class DeserializableColumns columns where
  deserializeRawColumns :: ProtocolRevision -> UVarInt -> Get columns

instance
  DeserializableColumns (Columns '[])
  where
  {-# INLINE deserializeRawColumns #-}
  deserializeRawColumns _rev _rows = pure Empty

instance
  ( KnownColumn (Column name chType)
  , DeserializableColumn (Column name chType)
  , DeserializableColumns (Columns extraColumns)
  )
  =>
  DeserializableColumns (Columns (Column name chType ': extraColumns))
  where
  {-# INLINE deserializeRawColumns #-}
  deserializeRawColumns rev rows =
    AddColumn
      <$> deserializeColumn rev rows
      <*> deserializeRawColumns @(Columns extraColumns) rev rows


-- ** Column deserialization

{-# SPECIALIZE replicateM :: Int -> Get chType -> Get [chType] #-}

class DeserializableColumn column where
  deserializeColumn :: ProtocolRevision -> UVarInt -> Get column

instance
  ( KnownColumn (Column name chType)
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name chType) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    column <- replicateM (fromIntegral rows) (deserialize @chType rev)
    pure $ mkColumn @(Column name chType) column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Deserializable chType
  ) =>
  DeserializableColumn (Column name (Nullable chType)) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    nulls <- replicateM (fromIntegral rows) (deserialize @ChUInt8 rev)
    nullable <-
      forM
        nulls
        (\case
          0 -> Just <$> deserialize @chType rev
          _ -> (Nothing <$ deserialize @chType rev)
        )
    pure $ mkColumn @(Column name (Nullable chType)) nullable

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Deserializable chType
  , ToChType (LowCardinality chType) chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  DeserializableColumn (Column name (LowCardinality chType)) where
  deserializeColumn rev rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    _serializationType <- (.&. 0xf) <$> deserialize @ChUInt64 rev
    _index_size <- deserialize @ChInt64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    lc <- replicateM (fromIntegral rows) (toChType <$> deserialize @chType rev)
    pure $ mkColumn @(Column name (LowCardinality chType)) lc

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (ChArray chType))
  , Deserializable chType
  , TypeError ('Text "Arrays deserialization still unsupported")
  )
  => DeserializableColumn (Column name (ChArray chType)) where
  deserializeColumn rev _rows = do
    _columnName <- deserialize @ChString rev
    _columnType <- deserialize @ChString rev
    _isCustom <- deserialize @(ChUInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
    (arraySize, _offsets) <- traceShowId <$> readOffsets rev
    _types <- replicateM (fromIntegral arraySize) (deserialize @chType rev)
    pure $ mkColumn @(Column name (ChArray chType)) []
    where
    readOffsets :: ProtocolRevision -> Get (ChUInt64, [ChUInt64])
    readOffsets revivion = do
      size <- deserialize @ChUInt64 rev
      (size, ) <$> go size
      where
      go arraySize =
        do
        nextOffset <- deserialize @ChUInt64 revivion
        if arraySize >= nextOffset
          then pure [nextOffset]
          else (nextOffset :) <$> go arraySize


class
  Deserializable chType
  where
  default deserialize :: (Generic chType, GDeserializable (Rep chType)) => ProtocolRevision -> Get chType
  deserialize :: ProtocolRevision -> Get chType
  deserialize rev = to <$> gDeserialize rev


-- ** Generics

class GDeserializable f
  where
  gDeserialize :: ProtocolRevision -> Get (f p)

instance
  GDeserializable f
  =>
  GDeserializable (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = M1 . M1 <$> gDeserialize rev

instance
  GDeserializable (left :*: (right1 :*: right2))
  =>
  GDeserializable ((left :*: right1) :*: right2)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = (\(l :*: (r1 :*: r2)) -> (l :*: r1) :*: r2) <$> gDeserialize rev

instance
  (GDeserializable (S1 metaSel field), GDeserializable right)
  =>
  GDeserializable (S1 metaSel field :*: right)
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev = (:*:) <$> gDeserialize rev <*> gDeserialize rev

instance
  Deserializable chType
  =>
  GDeserializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gDeserialize #-}
  gDeserialize rev =  M1 . K1 <$> deserialize @chType rev


-- ** Database types

instance Deserializable ChUUID where
  deserialize _ = MkChUUID <$> (flip Word128 <$> getWord64le <*> getWord64le)

instance Deserializable ChString where
  deserialize rev = do
    strSize <- fromIntegral <$> deserialize @UVarInt rev
    toChType <$> readN strSize (BS.take strSize)


instance Deserializable ChInt8 where deserialize _ = toChType <$> getInt8
instance Deserializable ChInt16 where deserialize _ = toChType <$> getInt16le
instance Deserializable ChInt32 where deserialize _ = toChType <$> getInt32le
instance Deserializable ChInt64 where deserialize _ = toChType <$> getInt64le
instance Deserializable ChInt128 where deserialize _ = toChType <$> (flip Int128 <$> getWord64le <*> getWord64le)
instance Deserializable ChUInt8 where deserialize _ = toChType <$> getWord8
instance Deserializable ChUInt16 where deserialize _ = toChType <$> getWord16le
instance Deserializable ChUInt32 where deserialize _ = toChType <$> getWord32le
instance Deserializable ChUInt64 where deserialize _ = toChType <$> getWord64le
instance Deserializable ChUInt128 where deserialize _ = toChType <$> (flip Word128 <$> getWord64le <*> getWord64le)
instance Deserializable ChDateTime where deserialize _ = toChType <$> getWord32le
instance Deserializable ChDate where deserialize _ = toChType <$> getWord16le

instance Deserializable UVarInt where
  deserialize _ = go 0 (0 :: UVarInt)
    where
    go i o | i < 10 = do
      byte <- getWord8
      let o' = o .|. ((fromIntegral byte .&. 0x7f) `unsafeShiftL` (7 * i))
      if byte .&. 0x80 == 0 then pure $! o' else go (i + 1) $! o'
    go _ _ = fail "input exceeds varuint size"








-- * Columns

-- ** Columns extraction helper

class
  HasColumns hasColumns
  where
  type GetColumns hasColumns :: [Type]

instance HasColumns (Columns columns)
  where
  type GetColumns (Columns columns) = columns


-- ** Take column by name from list of columns

type family
  TakeColumn (name :: Symbol) (columns :: [Type]) :: (Type, [Type])
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: (Type, [Type])
  where
  GoTakeColumn name (Column name chType ': columns) acc = '(Column name chType, acc ++ columns)
  GoTakeColumn name (Column name1 chType ': columns) acc = (GoTakeColumn name columns (Column name1 chType ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )

type family
  (++) (list1 :: [Type]) (list2 :: [Type]) :: [Type]
  where
  (++) '[]            list = list
  (++) (head ': tail) list = tail ++ (head ': list)

emptyColumns :: Columns '[]
emptyColumns = Empty

{-# INLINE [0] appendColumn #-}
appendColumn
  :: KnownColumn (Column name chType)
  => Column name chType
  -> Columns columns
  -> Columns (Column name chType ': columns)
appendColumn = AddColumn


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
  ChUInt8Column :: [ChUInt8] -> Column name ChUInt8
  ChUInt16Column :: [ChUInt16] -> Column name ChUInt16
  ChUInt32Column :: [ChUInt32] -> Column name ChUInt32
  ChUInt64Column :: [ChUInt64] -> Column name ChUInt64
  ChUInt128Column :: [ChUInt128] -> Column name ChUInt128
  ChInt8Column :: [ChInt8] -> Column name ChInt8
  ChInt16Column :: [ChInt16] -> Column name ChInt16
  ChInt32Column :: [ChInt32] -> Column name ChInt32
  ChInt64Column :: [ChInt64] -> Column name ChInt64
  ChInt128Column :: [ChInt128] -> Column name ChInt128
  ChDateColumn :: [ChDate] -> Column name ChDate
  ChDateTimeColumn :: [ChDateTime] -> Column name ChDateTime
  ChUUIDColumn :: [ChUUID] -> Column name ChUUID
  ChStringColumn :: [ChString] -> Column name ChString
  ChArrayColumn :: IsChType chType => [ChArray chType] -> Column name (ChArray chType)
  NullableColumn :: IsChType chType => [Nullable chType] -> Column name (Nullable chType)
  LowCardinalityColumn :: (IsLowCardinalitySupported chType, IsChType chType) => [chType] -> Column name (LowCardinality chType)

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

  mkColumn :: [GetColumnType column] -> Column (GetColumnName column) (GetColumnType column)

{-# INLINE [0] columnValues #-}
columnValues :: Column name chType -> [chType]
columnValues column = case column of
  (ChUInt8Column values) -> values
  (ChUInt16Column values) -> values
  (ChUInt32Column values) -> values
  (ChUInt64Column values) -> values
  (ChUInt128Column values) -> values
  (ChInt8Column values) -> values
  (ChInt16Column values) -> values
  (ChInt32Column values) -> values
  (ChInt64Column values) -> values
  (ChInt128Column values) -> values
  (ChDateColumn values) -> values
  (ChDateTimeColumn values) -> values
  (ChUUIDColumn values) -> values
  (ChStringColumn values) -> values
  (ChArrayColumn arrayValues) -> arrayValues
  (NullableColumn nullableValues) ->  nullableValues
  (LowCardinalityColumn lowCardinalityValues) -> map fromChType lowCardinalityValues

instance KnownSymbol name => KnownColumn (Column name ChUInt8) where mkColumn = ChUInt8Column
instance KnownSymbol name => KnownColumn (Column name ChUInt16) where mkColumn = ChUInt16Column
instance KnownSymbol name => KnownColumn (Column name ChUInt32) where mkColumn = ChUInt32Column
instance KnownSymbol name => KnownColumn (Column name ChUInt64) where mkColumn = ChUInt64Column
instance KnownSymbol name => KnownColumn (Column name ChUInt128) where mkColumn = ChUInt128Column
instance KnownSymbol name => KnownColumn (Column name ChInt8)  where mkColumn = ChInt8Column
instance KnownSymbol name => KnownColumn (Column name ChInt16) where mkColumn = ChInt16Column
instance KnownSymbol name => KnownColumn (Column name ChInt32) where mkColumn = ChInt32Column
instance KnownSymbol name => KnownColumn (Column name ChInt64) where mkColumn = ChInt64Column
instance KnownSymbol name => KnownColumn (Column name ChInt128) where mkColumn = ChInt128Column
instance KnownSymbol name => KnownColumn (Column name ChDate) where mkColumn = ChDateColumn
instance KnownSymbol name => KnownColumn (Column name ChDateTime) where mkColumn = ChDateTimeColumn
instance KnownSymbol name => KnownColumn (Column name ChUUID) where mkColumn = ChUUIDColumn
instance
  ( KnownSymbol name
  , IsChType chType
  , IsChType (Nullable chType)
  ) =>
  KnownColumn (Column name (Nullable chType)) where mkColumn = NullableColumn
instance KnownSymbol name => KnownColumn (Column name ChString) where mkColumn = ChStringColumn
instance
  ( KnownSymbol name
  , IsChType (LowCardinality chType)
  , IsLowCardinalitySupported chType
  ) =>
  KnownColumn (Column name (LowCardinality chType)) where mkColumn = LowCardinalityColumn . map fromChType
instance KnownSymbol name => KnownColumn (Column name (ChArray ChString)) where mkColumn = ChArrayColumn


-- ** Columns

instance
  Serializable (Columns '[])
  where
  {-# INLINE serialize #-}
  serialize _rev Empty = ""

instance
  ( Serializable (Columns columns)
  , Serializable col
  ) =>
  Serializable (Columns (col ': columns))
  where
  {-# INLINE serialize #-}
  serialize rev (AddColumn col columns) = serialize rev col <> serialize rev columns

instance
  ( KnownColumn (Column name chType)
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name chType) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name chType))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name chType))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> mconcat (Prelude.map (serialize @chType rev) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , IsChType chType
  , Serializable chType
  ) => Serializable (Column name (Nullable chType)) where
  {-# INLINE serialize #-}
  serialize rev column
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    -- Nulls
    <> mconcat (Prelude.map (serialize @ChUInt8 rev . maybe 1 (const 0)) (columnValues column))
    -- Values
    <> mconcat (Prelude.map (serialize @chType rev . maybe defaultValueOfTypeName id) (columnValues column))

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , IsChType chType
  , Serializable chType
  , TypeError ('Text "LowCardinality serialization still unsupported")
  ) => Serializable (Column name (LowCardinality chType)) where
  {-# INLINE serialize #-}
  serialize rev (LowCardinalityColumn column)
    =  serialize rev (toChType @ChString $ renderColumnName @(Column name (Nullable chType)))
    <> serialize rev (toChType @ChString $ renderColumnType @(Column name (Nullable chType)))
    -- serialization is not custom
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @ChUInt8 rev 0)
    <> undefined column








-- * Parameters

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
viewParameters :: (Parameters '[] -> Parameters passedParameters) -> Builder
viewParameters interpreter = "(" <> renderParameters (interpreter NoParameters) <> ")"

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
renderParameter (MkParamater chType) = (byteString . BS8.pack . symbolVal @name) Proxy <> "=" <> toQueryPart chType

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








-- * Serialization

-- *** Generic API

type GenericWritable record columns =
  ( Generic record
  , GWritable columns (Rep record)
  )

class
  ( HasColumns (Columns (GetColumns columns))
  , Serializable (Columns (GetColumns columns))
  , DeserializableColumns (Columns (GetColumns columns))
  ) =>
  WritableInto columns record
  where
  default serializeRecords :: GenericWritable record (GetColumns columns) => ProtocolRevision -> [record] -> Builder
  serializeRecords :: ProtocolRevision -> [record] -> Builder
  serializeRecords rev = gSerializeRecords @(GetColumns columns) rev . map from

  default writingColumns :: GenericWritable record (GetColumns columns) => Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns columns) @(Rep record)

  default columnsCount :: GenericWritable record (GetColumns columns) => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @(GetColumns columns) @(Rep record)

class GWritable (columns :: [Type]) f
  where
  gSerializeRecords :: ProtocolRevision -> [f p] -> Builder
  gWritingColumns :: Builder
  gColumnsCount :: UVarInt

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = gSerializeRecords @columns rev . map (unM1 . unM1)
  gWritingColumns = gWritingColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = gSerializeRecords @columns rev . map (\((l1 :*: l2) :*: r) -> l1 :*: (l2 :*: r))
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))
  gColumnsCount = gColumnsCount @columns @(left1 :*: (left2 :*: right))

instance
  ( GWritable '[Column name chType] (S1 (MetaSel (Just name) a b f) rec)
  , GWritable restColumns right
  , '(Column name chType, restColumns)~ TakeColumn name columns
  )
  =>
  GWritable columns (S1 (MetaSel (Just name) a b f) rec :*: right)
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev
    = (\(a, b) -> gSerializeRecords @'[Column name chType] rev a <> gSerializeRecords @restColumns rev b)
    . unzip . map (\(l :*: r) -> (l, r))
  gWritingColumns =
    gWritingColumns @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec)
    <> ", " <> gWritingColumns @restColumns @right
  gColumnsCount = gColumnsCount @'[Column name chType] @(S1 (MetaSel (Just name) a b f) rec) + gColumnsCount @restColumns @right

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name chType)
  , ToChType chType inputType
  , Serializable (Column name chType)
  , '(Column name chType, restColumns) ~ TakeColumn name columns
  ) =>
  GWritable columns (S1 (MetaSel (Just name) a b f) (Rec0 inputType))
  where
  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = serialize rev . mkColumn @(Column name chType) . map (toChType . unK1 . unM1)
  gWritingColumns = renderColumnName @(Column name chType)
  gColumnsCount = 1

class Serializable chType
  where
  default serialize :: (Generic chType, GSerializable (Rep chType)) => ProtocolRevision -> chType -> Builder
  serialize :: ProtocolRevision -> chType -> Builder
  serialize rev = gSerialize rev . from


-- ** Database types
instance Serializable UVarInt where
  serialize _ = go
    where
    go i
      | i < 0x80 = word8 (fromIntegral i)
      | otherwise = word8 (setBit (fromIntegral i) 7) <> go (unsafeShiftR i 7)

instance Serializable ChString where
  serialize rev str
    =  (serialize @UVarInt rev . fromIntegral . BS.length . fromChType) str
    <> (execPut . putByteString . fromChType) str

instance Serializable ChUUID where serialize _ = execPut . (\(hi, lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable ChInt8 where serialize _ = execPut . putInt8 . fromChType
instance Serializable ChInt16 where serialize _ = execPut . putInt16le . fromChType
instance Serializable ChInt32 where serialize _ = execPut . putInt32le . fromChType
instance Serializable ChInt64 where serialize _ = execPut . putInt64le . fromChType
instance Serializable ChInt128 where serialize _ = execPut . (\(Int128 hi lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable ChUInt8 where serialize _ = execPut . putWord8 . fromChType
instance Serializable ChUInt16 where serialize _ = execPut . putWord16le . fromChType
instance Serializable ChUInt32 where serialize _ = execPut . putWord32le . fromChType
instance Serializable ChUInt64 where serialize _ = execPut . putWord64le . fromChType
instance Serializable ChUInt128 where serialize _ = execPut . (\(Word128 hi lo) -> putWord64le lo <> putWord64le hi) . fromChType
instance Serializable ChDateTime where serialize _ = execPut . putWord32le . fromChType
instance Serializable ChDate where serialize _ = execPut . putWord16le . fromChType


-- ** Generics

class GSerializable f
  where
  gSerialize :: ProtocolRevision -> f p -> Builder

instance
  GSerializable f
  =>
  GSerializable (D1 c (C1 c2 f))
  where
  {-# INLINE gSerialize #-}
  gSerialize rev (M1 (M1 re)) = gSerialize rev re

instance
  GSerializable (left1 :*: (left2 :*: right))
  =>
  GSerializable ((left1 :*: left2) :*: right)
  where
  {-# INLINE gSerialize #-}
  gSerialize rev ((l1 :*: l2) :*: r) = gSerialize rev (l1 :*: (l2 :*: r))

instance
  Serializable chType
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType))
  where
  {-# INLINE gSerialize #-}
  gSerialize rev = serialize rev . unK1 . unM1

instance
  (Serializable chType, GSerializable right)
  =>
  GSerializable (S1 (MetaSel (Just typeName) a b f) (Rec0 chType) :*: right)
  where
  {-# INLINE gSerialize #-}
  gSerialize rev (left :*: right)
    = (serialize rev . unK1 . unM1 $ left) <> gSerialize rev right








-- * Versioning

client_version_major, client_version_minor :: UVarInt
client_version_patch :: UVarInt  `SinceRevision` DBMS_MIN_REVISION_WITH_VERSION_PATCH
client_version_major = case versionBranch version of (x:_) -> fromIntegral x; _ -> 0
client_version_minor = case versionBranch version of (_:x:_) -> fromIntegral x; _ -> 0
client_version_patch = MkSinceRevision $ case versionBranch version of (_:_:x:_) -> fromIntegral x; _ -> 0

client_name :: ChString
client_name = fromString $
  "ClickHaskell-"
  <> show client_version_major <> "."
  <> show client_version_minor <> "."
  <> show client_version_patch

newtype ProtocolRevision = MkProtocolRevision Word64
  deriving newtype (Show, Eq, Num, Ord)

instance Deserializable ProtocolRevision where deserialize = coerce <$> deserialize @UVarInt
instance Serializable ProtocolRevision where serialize rev = serialize @UVarInt rev . coerce

{-# INLINE [0] afterRevision #-}
afterRevision
  :: forall revision monoid
  .  (KnownNat revision, Monoid monoid)
  => ProtocolRevision -> monoid -> monoid
afterRevision chosenRevision monoid =
  if chosenRevision >= (fromIntegral . natVal) (Proxy @revision)
  then monoid
  else mempty

{-# INLINE [0] mostRecentRevision #-}
mostRecentRevision :: ProtocolRevision
mostRecentRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented
instance Show a => Show (SinceRevision a revisionNumber) where
  show (MkSinceRevision a) = show a
  show NotPresented = ""

instance
  ( KnownNat revision
  , Deserializable chType
  )
  =>
  Deserializable (SinceRevision chType revision)
  where
  deserialize rev =
    if rev >= (fromIntegral . natVal) (Proxy @revision)
    then MkSinceRevision <$> deserialize @chType rev
    else pure NotPresented

instance
  ( KnownNat revision
  , Serializable chType
  )
  =>
  Serializable (SinceRevision chType revision)
  where
  serialize rev (MkSinceRevision val) = afterRevision @revision rev (serialize rev val)
  serialize rev NotPresented          = afterRevision @revision rev (error "Unexpected error")


{-
  Slightly modified C++ sources:
  https://github.com/ClickHouse/ClickHouse/blob/eb4a74d7412a1fcf52727cd8b00b365d6b9ed86c/src/Core/ProtocolDefines.h#L6
-}
type DBMS_TCP_PROTOCOL_VERSION = 54448;

type DBMS_MIN_REVISION_WITH_CLIENT_INFO = 54032;
type DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE = 54058;
type DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO = 54060;
type DBMS_MIN_REVISION_WITH_TABLES_STATUS = 54226;
type DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE = 54337;
type DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME = 54372;
type DBMS_MIN_REVISION_WITH_VERSION_PATCH = 54401;
type DBMS_MIN_REVISION_WITH_SERVER_LOGS = 54406;
type DBMS_MIN_REVISION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 54448;
type DBMS_MIN_MAJOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 21;
type DBMS_MIN_MINOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD = 4;
type DBMS_MIN_REVISION_WITH_COLUMN_DEFAULTS_METADATA = 54410;
type DBMS_MIN_REVISION_WITH_LOW_CARDINALITY_TYPE = 54405;
type DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO = 54420;
type DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS = 54429;
type DBMS_MIN_REVISION_WITH_SCALARS = 54429;
type DBMS_MIN_REVISION_WITH_OPENTELEMETRY = 54442;
type DBMS_MIN_REVISION_WITH_AGGREGATE_FUNCTIONS_VERSIONING = 54452;
type DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION = 1;
type DBMS_MIN_SUPPORTED_PARALLEL_REPLICAS_PROTOCOL_VERSION = 3;
type DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_MARK_SEGMENT_SIZE_FIELD = 4;
type DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION = 4;
type DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS = 54453;
type DBMS_MERGE_TREE_PART_INFO_VERSION = 1;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET = 54441;
type DBMS_MIN_REVISION_WITH_X_FORWARDED_FOR_IN_CLIENT_INFO = 54443;
type DBMS_MIN_REVISION_WITH_REFERER_IN_CLIENT_INFO = 54447;
type DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH = 54448;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INCREMENTAL_PROFILE_EVENTS = 54451;
type DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION = 54454;
type DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME = 54449;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PROFILE_EVENTS_IN_INSERT = 54456;
type DBMS_MIN_PROTOCOL_VERSION_WITH_VIEW_IF_PERMITTED = 54457;
type DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY = 54458;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS = 54459;
type DBMS_MIN_PROTOCOL_VERSION_WITH_SERVER_QUERY_TIME_IN_PROGRESS = 54460;
type DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES = 54461;
type DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2 = 54462;
type DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS = 54463;
type DBMS_MIN_PROTOCOL_VERSION_WITH_TIMEZONE_UPDATES = 54464;
type DBMS_MIN_REVISION_WITH_SPARSE_SERIALIZATION = 54465;
type DBMS_MIN_REVISION_WITH_SSH_AUTHENTICATION = 54466;
type DBMS_MIN_REVISION_WITH_TABLE_READ_ONLY_CHECK = 54467;
type DBMS_MIN_REVISION_WITH_SYSTEM_KEYWORDS_TABLE = 54468;
type DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION = 54469;
type DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS = 54470;
type DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL = 54471;

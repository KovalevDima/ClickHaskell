{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ClickHaskell.Versioning
  {-# DEPRECATED "\
  \This module would be deleted in next minor release\
  \Please move its imports to ClickHaskell module\
  \" #-}
  ( ProtocolRevision(..)
  , mostRecentRevision
  , SinceRevision(..)
  , DBMS_TCP_PROTOCOL_VERSION

  , DBMS_MIN_REVISION_WITH_CLIENT_INFO
  , DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE
  , DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO
  , DBMS_MIN_REVISION_WITH_TABLES_STATUS
  , DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE
  , DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME
  , DBMS_MIN_REVISION_WITH_VERSION_PATCH
  , DBMS_MIN_REVISION_WITH_SERVER_LOGS
  , DBMS_MIN_REVISION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD
  , DBMS_MIN_MAJOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD
  , DBMS_MIN_MINOR_VERSION_WITH_CURRENT_AGGREGATION_VARIANT_SELECTION_METHOD
  , DBMS_MIN_REVISION_WITH_COLUMN_DEFAULTS_METADATA
  , DBMS_MIN_REVISION_WITH_LOW_CARDINALITY_TYPE
  , DBMS_MIN_REVISION_WITH_CLIENT_WRITE_INFO
  , DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
  , DBMS_MIN_REVISION_WITH_SCALARS
  , DBMS_MIN_REVISION_WITH_OPENTELEMETRY
  , DBMS_MIN_REVISION_WITH_AGGREGATE_FUNCTIONS_VERSIONING
  , DBMS_CLUSTER_PROCESSING_PROTOCOL_VERSION
  , DBMS_MIN_SUPPORTED_PARALLEL_REPLICAS_PROTOCOL_VERSION
  , DBMS_PARALLEL_REPLICAS_MIN_VERSION_WITH_MARK_SEGMENT_SIZE_FIELD
  , DBMS_PARALLEL_REPLICAS_PROTOCOL_VERSION
  , DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS
  , DBMS_MERGE_TREE_PART_INFO_VERSION
  , DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET
  , DBMS_MIN_REVISION_WITH_X_FORWARDED_FOR_IN_CLIENT_INFO
  , DBMS_MIN_REVISION_WITH_REFERER_IN_CLIENT_INFO
  , DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH
  , DBMS_MIN_PROTOCOL_VERSION_WITH_INCREMENTAL_PROFILE_EVENTS
  , DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION
  , DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME
  , DBMS_MIN_PROTOCOL_VERSION_WITH_PROFILE_EVENTS_IN_INSERT
  , DBMS_MIN_PROTOCOL_VERSION_WITH_VIEW_IF_PERMITTED
  , DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM
  , DBMS_MIN_PROTOCOL_VERSION_WITH_QUOTA_KEY
  , DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS
  , DBMS_MIN_PROTOCOL_VERSION_WITH_SERVER_QUERY_TIME_IN_PROGRESS
  , DBMS_MIN_PROTOCOL_VERSION_WITH_PASSWORD_COMPLEXITY_RULES
  , DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET_V2
  , DBMS_MIN_PROTOCOL_VERSION_WITH_TOTAL_BYTES_IN_PROGRESS
  , DBMS_MIN_PROTOCOL_VERSION_WITH_TIMEZONE_UPDATES
  , DBMS_MIN_REVISION_WITH_SPARSE_SERIALIZATION
  , DBMS_MIN_REVISION_WITH_SSH_AUTHENTICATION
  , DBMS_MIN_REVISION_WITH_TABLE_READ_ONLY_CHECK
  , DBMS_MIN_REVISION_WITH_SYSTEM_KEYWORDS_TABLE
  , DBMS_MIN_REVISION_WITH_ROWS_BEFORE_AGGREGATION
  , DBMS_MIN_PROTOCOL_VERSION_WITH_CHUNKED_PACKETS
  , DBMS_MIN_REVISION_WITH_VERSIONED_PARALLEL_REPLICAS_PROTOCOL
  ) where
import GHC.TypeLits (Nat, natVal)
import Data.Word (Word64)
import Data.Data (Proxy(..))

{-
  Copied from ClickHaskell module
  Just delete this file in next release. It shouldn't be exported
-}
{-# INLINE [0] mostRecentRevision #-}
mostRecentRevision :: ProtocolRevision
mostRecentRevision = (fromIntegral . natVal) (Proxy @DBMS_TCP_PROTOCOL_VERSION)

newtype ProtocolRevision = MkProtocolRevision Word64
  deriving newtype (Show, Eq, Num, Ord)

data SinceRevision a (revisionNumber :: Nat) = MkSinceRevision a | NotPresented

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

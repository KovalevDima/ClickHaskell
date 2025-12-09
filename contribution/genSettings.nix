{ pkgs, clickhouseRepo }:


let templ = ''
module ClickHaskell.Protocol.SettingsSupport where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Data.Binary (Get)
import Data.ByteString.Builder (Builder)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits


class
  (Serializable settType, ToQueryPart settType)
  =>
  IsSettingType settType
  where
  toSettingType :: settType -> SettingType
  fromSettingType :: SettingType -> settType

  settingToText :: SettingType -> ChString
  settingToText = toChType . toQueryPart @settType . fromSettingType

data SettingType where
  SettingUInt64 :: UInt64 -> SettingType
  SettingString :: ChString -> SettingType

instance IsSettingType ChString where
  toSettingType str = SettingString str
  fromSettingType (SettingString str) = str
  fromSettingType _ = error "Impossible"

instance IsSettingType UInt64 where
  toSettingType uint64 = SettingUInt64 uint64
  fromSettingType (SettingUInt64 uint64) = uint64
  fromSettingType _ = error "Impossible"


data SettingSerializer =
  MkSettingSerializer
    { deserializer :: ProtocolRevision -> Get SettingType
    , serializer   :: ProtocolRevision -> SettingType -> Builder
    }

class
  ( IsSettingType settType
  , KnownSymbol name
  )
  =>
  KnownSetting name settType | name -> settType
  where
  mkSettingSerializer :: (ChString, SettingSerializer)
  mkSettingSerializer =
    let name = toChType (symbolVal @name Proxy)
        deserializer = \rev ->
          if rev >= mkRev @DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
          then fail "Deserialization of Settings serializaed as strings is unsuported"
          else toSettingType <$> deserialize @settType rev
        serializer = \rev ->
          if rev >= mkRev @DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
          then (serialize @ChString rev . settingToText @settType)
          else serialize @settType rev . fromSettingType
    in (name, MkSettingSerializer{..})

data Setting (a :: Symbol) (settType :: Type)
'';

in

pkgs.stdenv.mkDerivation {
  pname = "supported-settings";
  version = "1.0";

  src = clickhouseRepo;

  buildInputs = with pkgs; [ gawk coreutils bash ];

  installPhase = ''
    mkdir -p $out
    outFile=$out/SettingsSupport.hs

    cat > $outFile << EOF
    ${templ}
    EOF

    gawk -f - "$src/src/Core/Settings.cpp" >> $outFile <<'AWK'
      BEGIN {
        settings_count = 0
        inst_count = 0
      }

      match($0, /DECLARE\(\s*([A-Za-z0-9_]+)\s*,\s*([A-Za-z0-9_]+)/, m) {
        typ = m[1]; name = m[2];
        htype = "";

        if (typ == "UInt64") { htype="UInt64" }
        else if (typ == "String") { htype="ChString" }

        # TODO: add support
        else if ( \
            typ == "Dialect" || \
            typ == "Float32" || \
            typ == "Float64" || \
            typ == "Seconds" || \
            typ == "Milliseconds" || \
            typ == "MaxThreads" || \
            typ == "LoadBalancing" || \
            typ == "NonZeroUInt64" || \
            typ == "Float" || \
            typ == "Int64" || \
            typ == "Int32" || \
            typ == "Bool" || \
            typ == "TotalsMode" || \
            typ == "DistributedProductMode" || \
            typ == "UInt64Auto" || \
            typ == "UpdateParallelMode" || \
            typ == "Map" || \
            typ == "JoinStrictness" || \
            typ == "BoolAuto" || \
            typ == "OverflowMode" || \
            typ == "OverflowModeGroupBy" || \
            typ == "Double" || \
            typ == "JoinAlgorithm" || \
            typ == "LogsLevel" || \
            typ == "GeoToH3ArgumentOrder" || \
            typ == "LightweightDeleteMode" || \
            typ == "MySQLDataTypesSupport" || \
            typ == "AlterUpdateMode" || \
            typ == "IcebergMetadataLogLevel" || \
            typ == "QueryResultCacheNondeterministicFunctionHandling" || \
            typ == "DefaultTableEngine" || \
            typ == "LogQueriesType" || \
            typ == "DistributedDDLOutputMode" || \
            typ == "SetOperationMode" || \
            typ == "DecorrelationJoinKind" || \
            typ == "ShortCircuitFunctionEvaluation" || \
            typ == "LocalFSReadMethod" || \
            typ == "DistributedCacheLogMode" || \
            typ == "DistributedCachePoolBehaviourOnLimit" || \
            typ == "DateTimeInputFormat" || \
            typ == "SQLSecurityType" || \
            typ == "ParallelReplicasMode" || \
            typ == "Timezone" || \
            typ == "VectorSearchFilterStrategy" || \
            typ == "TransactionsWaitCSNMode" || \
            typ == "QueryResultCacheSystemTableHandling" \
            ) { next }

        else {
          printf("ERROR: unsupported type \"%s\" for setting \"%s\"\n", typ, name) > "/dev/stderr"
          exit 1
        }

        instances[inst_count++] = \
          "instance KnownSetting \"" name "\" " htype

        settings[settings_count++] = \
          "  Setting \"" name "\" " htype
      }

      END {
        for (i = 0; i < inst_count; ++i) {
          print instances[i]
        }

        print ""
        print "type SupportedSettings = '["
        for (i = 0; i < settings_count; ++i) {
          line = settings[i]
          if (i < settings_count - 1) line = line ","
          print line
        }
        print "  ]"
      }
    AWK
  '';
}

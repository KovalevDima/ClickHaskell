{ pkgs, clickhouseRepo }:

pkgs.stdenv.mkDerivation {
  pname = "supported-settings";
  version = "1.0";

  src = clickhouseRepo;

  buildInputs = with pkgs; [ gawk coreutils bash ];

  installPhase = ''
    mkdir -p $out
    outFile=$out/SettingsSupport.hs

    cat > $outFile << EOF
    module ClickHaskell.Packets.SettingsSupport where

    -- Internal
    import ClickHaskell.Primitive

    -- GHC
    import Data.Kind
    import GHC.TypeLits

    data Setting (a :: Symbol) (settType :: Type)
    
    type SupportedSettings = '[
    EOF

    gawk '
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

        if (htype != "") {
          printf "    Setting \"%s\" %s,\n", name, htype
        }
      }

    ' "$src/src/Core/Settings.cpp" | sed '$ s/,$//' >> $outFile

    echo "  ]" >> $outFile
  '';
}

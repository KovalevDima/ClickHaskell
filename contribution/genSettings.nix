{ pkgs, clickhouseRepo }:

pkgs.stdenv.mkDerivation {
  pname = "supported-settings";
  version = "1.0";

  src = clickhouseRepo;

  # Нужные утилиты для генерации
  buildInputs = with pkgs; [ gawk coreutils bash ];

  installPhase = ''
    mkdir -p $out
    outFile=$out/SupportedSettings.hs

    echo "type SupportedSettings = '[" > $outFile

    gawk '
      match($0, /DECLARE\(\s*([A-Za-z0-9_]+)\s*,\s*([A-Za-z0-9_]+)/, m) {
        typ = m[1]; name = m[2];
        htype = "";

        if (typ == "UInt64") { htype="UInt64" }
        else if (typ == "Int32") { htype="Int32" }
        else if (typ == "String") { htype="String" }

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
            typ == "Bool" \
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

{pkgs, distPackage}:
with pkgs.haskell.lib;
pkgs.runCommand "${distPackage.name}-dist" {} ''
  mkdir $out
  mkdir -m 777 $out/packages $out/docs
  cp -r ${sdistTarball distPackage}/${distPackage.name}.tar.gz $out/packages
  cp -r ${documentationTarball distPackage}/${distPackage.name}-docs.tar.gz $out/docs
''

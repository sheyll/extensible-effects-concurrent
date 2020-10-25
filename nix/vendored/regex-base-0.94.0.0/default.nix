{ mkDerivation, array, base, bytestring, containers, mtl, stdenv
, text
}:
mkDerivation {
  pname = "regex-base";
  version = "0.94.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring containers mtl text
  ];
  homepage = "https://wiki.haskell.org/Regular_expressions";
  description = "Common \"Text.Regex.*\" API for Regex matching";
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, array, base, bytestring, containers, regex-base
, stdenv
}:
mkDerivation {
  pname = "regex-posix";
  version = "0.96.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring containers regex-base
  ];
  description = "POSIX Backend for \"Text.Regex\" (regex-base)";
  license = stdenv.lib.licenses.bsd3;
}

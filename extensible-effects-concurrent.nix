{ mkDerivation, base, containers, data-default, deepseq, directory
, extensible-effects, filepath, HUnit, lens, logging-effect
, monad-control, mtl, parallel, process, QuickCheck, random, stdenv
, stm, tagged, tasty, tasty-discover, tasty-hunit, time
, transformers
}:
mkDerivation {
  pname = "extensible-effects-concurrent";
  version = "0.5.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers data-default deepseq directory extensible-effects
    filepath lens logging-effect monad-control mtl parallel process
    QuickCheck random stm tagged time transformers
  ];
  testHaskellDepends = [
    base containers deepseq extensible-effects HUnit lens QuickCheck
    stm tasty tasty-discover tasty-hunit
  ];
  testToolDepends = [ tasty-discover ];
  homepage = "https://github.com/sheyll/extensible-effects-concurrent#readme";
  description = "Message passing concurrency as extensible-effect";
  license = stdenv.lib.licenses.bsd3;
}

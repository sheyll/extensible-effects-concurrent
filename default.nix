{ pkgs ? (import nix/pkgs.nix { })
, withProfiling ? false
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "extensible-effects-concurrent";
    src = ./.;
  };
  projectFileName = "cabal.project";
  # compiler-nix-name = "ghc8102";
  compiler-nix-name = "ghc865";
  pkg-def-extras = [ ];
  modules =
    [
      {
        packages.extensible-effects-concurrent.components.library.doCoverage = false;
      }
    ] ++
    (if withProfiling then
      [{
        packages.extensible-effects-concurrent.components.library.enableLibraryProfiling = true;
        packages.extensible-effects-concurrent.components.exes.extensible-effects-concurrent-example-1.enableExecutableProfiling = true;
        packages.extensible-effects-concurrent.components.exes.extensible-effects-concurrent-example-2.enableExecutableProfiling = true;
        packages.extensible-effects-concurrent.components.exes.extensible-effects-concurrent-example-3.enableExecutableProfiling = true;
        packages.extensible-effects-concurrent.components.exes.extensible-effects-concurrent-example-4.enableExecutableProfiling = true;
        packages.extensible-effects-concurrent.components.exes.extensible-effects-concurrent-example-loadtest.enableExecutableProfiling = true;
        packages.extensible-effects-concurrent.components.tests.extensible-effects-concurrent-test.enableExecutableProfiling = true;
      }] else [ ]);

}


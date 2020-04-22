let
  pkgs = import ./pkgs.nix;
  lib = pkgs.lib;
  haskellPackages = pkgs.haskellPackages;
  cleanSrc =
    lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" ".md" ".yml" ".yaml" ]
      // { name = "extensible-effects-concurrent"; };

  in haskellPackages.callCabal2nix
       "extensible-effects-concurrent" cleanSrc {}

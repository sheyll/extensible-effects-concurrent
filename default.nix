{lib, callCabal2nix}:
let
  cleanSrc =
    lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" ".md" ".yml" ".yaml" ]
      // { name = "extensible-effects-concurrent"; };

  in callCabal2nix "extensible-effects-concurrent" cleanSrc {}

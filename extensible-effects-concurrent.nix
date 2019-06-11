let
  pkgs = import ./pkgs.nix;
  lib = pkgs.lib;
  haskellPackages = pkgs.haskellPackages;
  cleanSrc = lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(lib.hasPrefix ".ghc.environment." base) &&
         !(lib.hasSuffix ".nix" base)
    );
    src = lib.cleanSource ./.;
  };

  in haskellPackages.callCabal2nix
       "extensible-effects-concurrent" cleanSrc {}

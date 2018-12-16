{ pkgs, ... }:
let
  cleanSrc = pkgs.pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(pkgs.pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(pkgs.pkgs.lib.hasSuffix ".nix" base)
    );
    src = pkgs.pkgs.lib.cleanSource ./.;
  };

  in pkgs.haskellPackages.callCabal2nix
       "extensible-effects-concurrent" cleanSrc {}

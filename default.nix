{ q ? import <nixos> {  }
, compiler ? "ghc843" }:
let
  cleanSrc = q.pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(q.pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(q.pkgs.lib.hasSuffix ".nix" base)
    );
    src = q.pkgs.lib.cleanSource ./.;
  };

  in q.pkgs.haskell.packages.${compiler}.callCabal2nix
       "extensible-effects-concurrent" cleanSrc {}

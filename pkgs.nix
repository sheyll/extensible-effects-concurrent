let

   config = c: c // {
      allowUnfree = true;
      packageOverrides = pkgs: {
        haskellPackages = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: {
            extensible-effects = super.callHackage "extensible-effects" "5.0.0.1" {};
          };
        };
      };
    };
 in
 # import <nixpkgs> { inherit config; }
 import (fetchTarball https://hydra.nixos.org/build/94660708/download/1/nixpkgs-19.09pre182505.147d17e5708.tar.xz) {inherit config;}


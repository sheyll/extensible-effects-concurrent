let
   # compiler = "ghc883";
   compiler = "ghc865";
   config = c: c // {
      allowUnfree = true;
      packageOverrides = pkgs: {
        haskellPackages = pkgs.haskell.packages."${compiler}".override {
          overrides = self: super: {
            extensible-effects = super.callHackage "extensible-effects" "5.0.0.1" {};
          };
        };
      };
    };
in
import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-20.03";
  url = "https://github.com/nixos/nixpkgs-channels/";
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-20.03`
  ref = "refs/heads/nixos-20.03";
  rev = "1e90c46c2d98f9391df79954a74d14f263cad729";
}) {inherit config;}

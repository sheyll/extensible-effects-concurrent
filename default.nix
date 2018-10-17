{ nixpkgs ? import <nixos> {}, compiler ? "ghc843" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./extensible-effects-concurrent.nix {}


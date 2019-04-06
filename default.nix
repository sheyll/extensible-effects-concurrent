{ pkgs ? (import <nixpkgs> {}) }:
pkgs.callPackage ./extensible-effects-concurrent.nix {}

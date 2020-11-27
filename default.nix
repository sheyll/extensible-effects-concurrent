{ pkgs ? (import nix/pkgs.nix { }) }:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "extensible-effects-concurrent";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8102";
  pkg-def-extras = [ ];
}


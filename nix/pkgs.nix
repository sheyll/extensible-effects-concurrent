# This file contains a ready-to-use 'nixpkgs' with the
# sources from the niv based 'sources.nix' and the 
# 'overlay.nix' applied.
let
  sources = import ./sources.nix {};
  haskellStuff = import ./haskellStuff.nix sources;
  g = import ./ghcide.nix;
  overlays = [ haskellStuff g ];
  config = { allowUnfree = true; };
in
  import sources.nixpkgs { inherit config overlays; }

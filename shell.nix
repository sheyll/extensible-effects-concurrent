let
  pkgs = import nix/pkgs.nix { };
in
(import ./default.nix { inherit pkgs; }).shellFor {
  packages = p: [ p.extensible-effects-concurrent ];
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    ormolu = "0.1.4.1";
    haskell-language-server = "0.6.0";
  };
  buildInputs = with pkgs.haskellPackages;
    [
      pkgs.emacs
      pkgs.neovim
      tasty-discover
      ghcid
      graphmod
      cabal-plan
      brittany
      weeder
      pkgs.graphviz-nox
      pkgs.pandoc
    ];
}


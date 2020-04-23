let
  extensible-effects-concurrent = import ./extensible-effects-concurrent.nix;
  pkgs = import ./pkgs.nix;
in
pkgs.haskellPackages.shellFor {
      packages = p: [extensible-effects-concurrent];
      withHoogle = true;
      buildInputs = with pkgs.haskellPackages;
                    [ pkgs.cabal-install
                      pkgs.cabal2nix
                      pkgs.erlang
                      ghcid
                      hoogle
                      graphmod
                      cabal-plan
                      ormolu
                      # brittany
                      weeder
                      pkgs.nix
                      pkgs.graphviz-nox
                    ];
      }

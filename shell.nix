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
                      pointfree
                      graphmod
                      cabal-plan
                      # brittany
                      weeder
                      pkgs.stack
                      pkgs.nix
                      pkgs.graphviz-nox
                    ];
      }

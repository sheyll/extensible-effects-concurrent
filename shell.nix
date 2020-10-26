let
  overlay = import ./overlay.nix;
  withHoogle = import ./nix/with-hoogle.nix;
  pkgs = 
    (((import ./nix/pkgs.nix).
      extend overlay).
      extend withHoogle);
in
pkgs.haskellPackages.shellFor {
      packages = p: [pkgs.haskellPackages.extensible-effects-concurrent];
      withHoogle = true;
      buildInputs = with pkgs.haskellPackages;
                    [ pkgs.cabal-install
                      pkgs.cabal2nix
                      pkgs.erlang
                      ghcide
                      ghcid
                      hoogle
                      graphmod
                      cabal-plan
                      ormolu
                      # brittany
                      weeder
                      pkgs.nix
                      pkgs.graphviz-nox
                      pkgs.niv
                    ];
      }

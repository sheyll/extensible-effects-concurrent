let
  overlay = import ./overlay.nix;
  withHoogle = import ./nix/with-hoogle.nix;
  ghcide = import ./nix/ghcide.nix;
  neovim = import ./nix/neovim.nix;
  pkgs = 
    (((import ./nix/pkgs.nix).
      extend overlay).
      extend neovim);
in
pkgs.eec.haskellPackages.shellFor {
      packages = p: [pkgs.eec.haskellPackages.extensible-effects-concurrent];
      withHoogle = true;
      buildInputs = with pkgs.eec.haskellPackages;
                    [ pkgs.cabal-install
                      pkgs.cabal2nix
                      pkgs.erlang
                      pkgs.eec.haskellPackages.ghcide
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

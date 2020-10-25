let
  overlay = import ./overlay.nix;
  pkgs = (import ./nix/pkgs.nix).extend overlay;
  haskellPackages = pkgs.eec.haskellPackages;
in 
  pkgs.callPackage 
    ./extensible-effects-concurrent.nix  
    { 
      callCabal2nix = haskellPackages.callCabal2nix; 
    }

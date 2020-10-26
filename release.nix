let
  pkgs = import ./nix/pkgs.nix;
  haskellPackages = pkgs.haskellPackages;
  hsLib = pkgs.haskell.lib;
in 
    hsLib.dontHaddock (hsLib.dontCheck (import ./. 
    { callCabal2nix = haskellPackages.callCabal2nix;
      lib = pkgs.lib;
    }))

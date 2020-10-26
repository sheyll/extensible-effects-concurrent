
self: super: {
    haskellPackages = 
      (super.haskellPackages or super.haskell.packages.ghc865).override
        (old: {
          overrides =
            self.lib.composeExtensions
              (old.overrides or (_: _: {}))
              (hself: hsuper:
                {
                 extensible-effects-concurrent = self.callPackage ./default.nix 
                  { callCabal2nix = hself.callCabal2nix; };
                });
        });
  }


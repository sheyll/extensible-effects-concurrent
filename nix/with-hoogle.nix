# Use `ghc.WithHoogle` as `ghc` in `haskellPackages`
self: super: {
    haskellPackages = super.haskellPackages.override
        (old: {
          overrides =
            super.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (hself: hsuper: {
              ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
              ghcWithPackages = hself.ghc.withPackages;
            });
          });
}

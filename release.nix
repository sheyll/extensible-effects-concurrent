{ withHoogle ? false
, nixVersion ? ./nixos-18-09.json
}:
let
  pinnedPkgs = import ./pkgs-from-json.nix { json = nixVersion; };


  customHaskellPackages = pinnedPkgs.haskellPackages.override (old: {
    overrides = pinnedPkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {

    extensible-effects-concurrent = self.callPackage ./default.nix { };

      # additional overrides go here
    });
  });

  hoogleAugmentedHaskellPackages =
    if withHoogle
      then import ./with-hoogle.nix { input = customHaskellPackages; }
      else customHaskellPackages;

in
  { extensible-effects-concurrent = customHaskellPackages.extensible-effects-concurrent;
  }

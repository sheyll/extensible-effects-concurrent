{

# Library of functions to use, for composeExtensions.
lib ? (import <nixpkgs> {}).pkgs.lib

# Input set of all haskell packages. A valid input would be:
# (import <nixpkgs> {}).pkgs.haskellPackages
input

}:

input.override
  (old: {
    overrides =
      lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      });
  })

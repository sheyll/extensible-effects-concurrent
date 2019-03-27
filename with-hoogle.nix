# Use `ghc.WithHoogle` as `ghc` in `haskellPackages`
{

# Library of functions to use, for composeExtensions.
lib ? (import <nixpkgs> {}).pkgs.lib

# Input set of all haskell packages. A valid input would be:
# (import <nixpkgs> {}).pkgs.haskellPackages
haskellPackages ? (import <nixpkgs> {}).pkgs.haskellPackages

}:

haskellPackages.override
  (old: {
    overrides =
      lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
        ghcWithPackages = self.ghc.withPackages;
      });
  })

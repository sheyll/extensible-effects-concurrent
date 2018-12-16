let
  extensible-effects-concurrent-derivation =
    (import ./release.nix {
      withHoogle = true;
      nixVersion = ./nixos-unstable.json;
    }).extensible-effects-concurrent;
in
  extensible-effects-concurrent-derivation.env

# A Nix package set explicitly for cross compiling to
# static musl-libc executables. 
#
# This file contains IOHKs haskell.nix configured 
# to cross compile to static executables based on musl.
# It reads sources from the niv based 'sources.nix'.
import ./pkgs.nix {
  selectCrossPkgs = pkgs: pkgs.pkgsCross.musl64;
  crossConfig = {
    crossSystem = { config = "x86_64-unknown-linux-musl"; };
  };
}


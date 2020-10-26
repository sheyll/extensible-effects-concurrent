pself: psuper: {
    haskellPackages = psuper.haskellPackages.override 
    (old: {
      overrides = 
       pself.lib.composeExtensions 
       (old.overrides or (_:_:{}))
       (let
            dc = psuper.haskell.lib.dontCheck;
        in self: super: {
          ghcide = dc (self.callCabal2nix "ghcide" 
            (fetchTarball { 
              url = "https://github.com/digital-asset/ghcide/tarball/v0.2.0"; 
              sha256 = "1d5jpff480p9i2fypxp9cmjjbivi1fy1lw3xhimq7iizwywdnsxv"; 
            }) {});
          ghc-check = dc (self.callCabal2nix "ghc-check" 
            (fetchTarball { 
              url = "https://hackage.haskell.org/package/ghc-check-0.3.0.1/ghc-check-0.3.0.1.tar.gz"; 
              sha256 = "1dj909m09m24315x51vxvcl28936ahsw4mavbc53danif3wy09ns"; 
            }) {});
          haddock-library = dc (self.callCabal2nix "haddock-library" 
            (fetchTarball { 
              url = "http://hackage.haskell.org/package/haddock-library-1.9.0/haddock-library-1.9.0.tar.gz"; 
              sha256 = "12nr4qzas6fzn5p4ka27m5gs2rym0bgbfrym34yp0cd6rw9zdcl3"; 
            }) {});
          hie-bios = dc (self.callCabal2nix "hie-bios" 
            (fetchTarball { 
              url = "http://hackage.haskell.org/package/hie-bios-0.5.1/hie-bios-0.5.1.tar.gz"; 
              sha256 = "0ff05109yraqcj2q8z2c1bn8q53b0dv1zxicjz01ivly0yx3pyln"; 
            }) {});
          haskell-lsp = dc (self.callCabal2nix "haskell-lsp" 
            (fetchTarball { 
              url = "http://hackage.haskell.org/package/haskell-lsp-0.22.0.0/haskell-lsp-0.22.0.0.tar.gz"; 
              sha256 = "1q3w46qcvzraxgmw75s7bl0qvb2fvff242r5vfx95sqska566b4m"; 
            }) {});
          haskell-lsp-types = dc (self.callCabal2nix "haskell-lsp-types" 
            (fetchTarball { 
              url = "http://hackage.haskell.org/package/haskell-lsp-types-0.22.0.0/haskell-lsp-types-0.22.0.0.tar.gz"; 
              sha256 = "1apjclphi2v6ggrdnbc0azxbb1gkfj3x1vkwpc8qd6lsrbyaf0n8"; 
            }) {});
          regex-tdfa = dc (self.callCabal2nix "regex-tdfa" 
            (fetchTarball { 
              url = "http://hackage.haskell.org/package/regex-tdfa-1.3.1.0/regex-tdfa-1.3.1.0.tar.gz"; 
              sha256 = "1a0l7kdjzp98smfp969mgkwrz60ph24xy0kh2dajnymnr8vd7b8g"; 
            }) {});
          regex-base = dc (self.callPackage ./vendored/regex-base-0.94.0.0 {});
          regex-posix = dc (self.callPackage ./vendored/regex-posix-0.96.0.0 {});
          monoid-subclasses = dc super.monoid-subclasses;
        });
      });
}

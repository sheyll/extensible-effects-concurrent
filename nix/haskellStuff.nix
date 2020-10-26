sources: self: super:
{
    haskellPackages = super.haskell.packages.ghc865.override
    (old: {
      overrides =
        self.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (hself: hsuper:
          {
             newtype-zoo = hsuper.callCabal2nix
               "newtype-zoo" sources.newtype-zoo {};
             hinterface = hself.callCabal2nix
               "hinterface" sources.hinterface {};
             extensible-effects = hself.callCabal2nix
               "extensible-effects" sources.extensible-effects {};
          });
    });
    # The ekg assets are the accompanieing 
    # HTML, CSS and Javascript files used by
    # the web-frontend of the Haskell 
    # in process monitoring library `ekg`.
    ekg_assets = 
      self.runCommand "ekg_assets" {
        EKG_DATA = self.eec.haskellPackages.ekg.data;
      }
      ''
        mkdir -p $out/share/ekg/
        find $EKG_DATA \
          -wholename '**ekg**assets' -type d \
          -exec cp -rv '{}' $out/share/ekg \;
        echo "Installed EKG assets to $out/share/ekg"
        find $out/share/ekg
        '';


    # Reflect the sources
    # ===================
    inherit sources;
}



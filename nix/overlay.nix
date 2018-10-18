final:
  previous:
    with final.haskell.lib;
    {
      prettyRelativeTimePackages = 
        { pretty-relative-time = failOnAllWarnings (final.haskellPackages.callCabal2nix "pretty-relative-time" (../.) {});
        };
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super: final.prettyRelativeTimePackages
        );
      });
    }

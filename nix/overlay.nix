final: prev:
with final.haskell.lib;
{
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        pretty-relative-time = buildStrictly (self.callPackage ../pretty-relative-time { });
      }
    );
  });
}

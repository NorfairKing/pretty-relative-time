final:
previous:
with final.haskell.lib;
{
  prettyRelativeTimePackages =
    {
      pretty-relative-time = buildStrictly (final.haskellPackages.callCabal2nixWithOptions "pretty-relative-time" (final.gitignoreSource ../.) "--no-hpack" { });
    };
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: final.prettyRelativeTimePackages
    );
  });
}

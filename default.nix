let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/validity-version.nix)
    + "/nix/overlay.nix")
  );
in (pkgsv {
  overlays = [
    validity-overlay
    (import ./nix/gitignore-src.nix)
    (import ./nix/overlay.nix)
  ];
  config.allowUnfree = true;
}).prettyRelativeTimePackages

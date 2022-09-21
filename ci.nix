{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, pkgs ? import ./nix/pkgs.nix { inherit sources nixpkgs system; }
}:
let
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  versions = {
    "nixos-22_05" = sources.nixpkgs-22_05;
    "nixos-21_11" = sources.nixpkgs-21_11;
    "nixos-21_05" = sources.nixpkgs-21_05;
  };

  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs system;
      };

    in
    p.prettyRelativeTimeRelease.overrideAttrs (old: { name = "pretty-relative-time-${version}"; });
in
{
  release = (import ./nix/pkgs.nix { inherit sources; }).prettyRelativeTimeRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { }).run;
  hoogle = pkgs.buildEnv {
    name = "pretty-relative-time-hoogle";
    paths = [ (pkgs.haskellPackages.ghcWithHoogle (ps: pkgs.lib.attrValues pkgs.prettyRelativeTimePackages)) ];
  };
  shell = pkgs.symlinkJoin {
    name = "pretty-relative-time-shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
} // builtins.mapAttrs mkReleaseForVersion versions

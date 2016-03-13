{ nixpkgs ? import <custom_pkgs> {}, compiler ? "ghc7102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./github-review.nix {
  buildTools = [ nixpkgs.dwilsonDevelopment nixpkgs.pkgs.haskell.packages.${compiler}.cabal-install ];
}

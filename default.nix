{ nixpkgs ? import ./nixpkgs.nix }:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages lib;

  filterSrc = import ./filter.nix { inherit nixpkgs; };

  drv = filterSrc (haskellPackages.callPackage ./package.nix {});

in

  drv

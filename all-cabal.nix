{ nixpkgs ? import ./nixpkgs.nix }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  filterSrc = import ./filter.nix { inherit nixpkgs; };
  cabal-nix =
    lib.fold
      (f: x: f x)
      (pkgs.haskellPackages.callPackage ./package.nix {})
      [
        haskellLib.justStaticExecutables
        filterSrc
      ];
  shared = import ./all-cabal-nix/Zwaluw/Zwaluw-0.1.nix;
  finalize = cabalFile:
    let env = { inherit cabalFile; buildInputs = [ cabal-nix ]; }; in
    pkgs.runCommand "cabal-nix-finalize.nix" env ''
      cabal-nix-finalize \
        --platform x86_64-linux \
        --compiler ghc \
        "$cabalFile" \
        >"$out"
    '';
  Cabal = { name, version }: {
    fromHackage = { revision, sha256 }:
      let r = toString revision; in
      let hackage = "http://hackage.haskell.org"; in
      pkgs.fetchurl {
        url = "${hackage}/package/${name}-${version}/revision/${r}.cabal";
        name = "${name}-${version}-r${r}.cabal";
        inherit sha256;
      };
  };
  Src = { name, version }: {
    fromHackage = sha256:
      pkgs.fetchurl {
        url = "mirror://hackage/${name}-${version}.tar.gz";
        inherit sha256;
      };
  };
  cabal = shared.cabal (Cabal shared.package);
  src = shared.src (Src shared.package);
  final = import (finalize cabal);
  components =
    {}
    // final.libraries
    // final.executables
    // final.tests
    // final.benchmarks;
  buildDepends =
    lib.mapAttrs (dep: _: pkgs.haskellPackages.${dep})
    (lib.foldl (l: r: l // r.buildDepends) {} (lib.attrValues components));
  toolDepends =
    lib.mapAttrs (dep: _: pkgs.haskellPackages.${dep})
    (lib.foldl (l: r: l // r.toolDepends) {} (lib.attrValues components));
  pkgconfigDepends =
    lib.mapAttrs (dep: _: pkgs.${dep})
    (lib.foldl (l: r: l // r.pkgconfigDepends) {} (lib.attrValues components));
  descr = {
    inherit (shared) package license homepage synopsis;
    inherit cabal src;
    inherit buildDepends toolDepends pkgconfigDepends;
  };
in

descr

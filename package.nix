{ mkDerivation, aeson, async, base, bytestring, Cabal, containers
, directory, filepath, hnix, hpack, monads-tf, optparse-applicative
, prettyprinter, process, stdenv, text, unordered-containers, yaml
}:
mkDerivation {
  pname = "cabal-nix";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring Cabal containers filepath hnix
    optparse-applicative process text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson async base bytestring Cabal containers directory filepath
    hnix monads-tf optparse-applicative prettyprinter process text
    unordered-containers yaml
  ];
  license = stdenv.lib.licenses.ncsa;
}

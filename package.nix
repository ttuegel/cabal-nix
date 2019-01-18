{ mkDerivation, aeson, async, base, bytestring, Cabal, containers
, directory, filepath, hnix, monads-tf, optparse-applicative
, prettyprinter, process, stdenv, text
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
  executableHaskellDepends = [
    aeson async base bytestring Cabal containers directory filepath
    hnix monads-tf optparse-applicative prettyprinter process text
  ];
  license = stdenv.lib.licenses.ncsa;
}

{ mkDerivation, base, colour, containers, diagrams-core
, diagrams-lib, ghcjs-base, ghcjs-dom, lens, linear, mtl
, NumInstances, stdenv, text
}:
mkDerivation {
  pname = "diagrams-ghcjs";
  version = "0.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base colour containers diagrams-core diagrams-lib ghcjs-base lens
    linear mtl NumInstances text
  ];
  executableHaskellDepends = [
    base diagrams-core diagrams-lib ghcjs-base ghcjs-dom mtl text
  ];
  homepage = "http://projects.haskell.org/diagrams/";
  description = "GHCJS backend for diagrams drawing EDSL";
  license = stdenv.lib.licenses.bsd3;
}

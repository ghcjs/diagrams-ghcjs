{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages_ghcjs }:

let 
  hs = haskellPackages.override {
        extension = self: super: rec {
          hsPkg = pkg: version: self.callPackage "/home/bergey/code/nixHaskellVersioned/${pkg}/${version}.nix" {};
          ghcjsCanvas = self.callPackage ../../ghcjs-canvas {};
          ghcjsJquery =  self.callPackage ../../ghcjs-jquery {};
          diagramsGhcjs = self.callPackage ../. {};
          thisPackage = self.callPackage ./. {};
      };
    };
  in
      pkgs.lib.overrideDerivation hs.thisPackage (attrs: {
       buildInputs = [hs.cabalInstall pkgs.gnumake ] ++ attrs.buildInputs;
 })

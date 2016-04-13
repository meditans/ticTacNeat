{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cereal, containers, hspec
      , lens, MonadRandom, neet, QuickCheck, random-shuffle, stdenv
      , vector
      }:
      mkDerivation {
        pname = "ticTacNeat";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring cereal containers lens MonadRandom neet
          random-shuffle vector
        ];
        testHaskellDepends = [ base hspec QuickCheck ];
        homepage = "http://github.com/meditans/ticTacNeat#readme";
        description = "Personal project";
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

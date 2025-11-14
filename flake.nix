{
  description = "Agda dependencies";

  inputs = {
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    souffle = {
      url = "github:souffle-lang/souffle/ea4ebd45b56619f9362d5db9bb6b7783b4bbb24c";
      flake = false;
    };
    souffle-haskell = {
      url = "github:luc-tielen/souffle-haskell";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    let
      agda-deps-overlay = final: prev: {
        souffle = prev.souffle.overrideAttrs (oldAttrs: {
          version = "custom";
          src = inputs.souffle;
          env = {
            CXXFLAGS = "-Wno-deprecated-declarations";
          };
          patches = [];
          postFixup = "";
        });

        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: {
            souffle-haskell =
              prev.haskell.lib.overrideCabal hprev.souffle-haskell (oldAttrs:
                {
                  src = inputs.souffle-haskell;
                  env = {
                    DATALOG_DIR = "./tests/fixtures/";
                  };
                  patches = [ ./nix/souffle-haskell.cabal.patch ];
                  broken = false;
                  testHaskellDepends = oldAttrs.testHaskellDepends ++ [ final.souffle prev.which ];
                });
              agda-deps = hfinal.callCabal2nix "agda-deps" ./. { };
           };
        };
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            agda-deps-overlay
          ];
        };
      in
      {
        packages = {
          default = pkgs.haskell.packages.ghc967.agda-deps;
        };
        devShells = {
          default = pkgs.haskell.packages.ghc967.shellFor {
            packages = hpkgs: [ hpkgs.agda-deps ];
            nativeBuildInputs =
                with pkgs.haskell.packages.ghc967;
                [
                  cabal-install
                  haskell-language-server
                  pkgs.xdot
                  pkgs.zlib
                  pkgs.souffle
                ];
          };
        };
      }
    );
}

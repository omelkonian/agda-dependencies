{
  description = "Agda dependencies";

  inputs.nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    let
      agda-dependencies-overlay = final: prev: {
        haskellPackages = prev.haskellPackages.extend (
          hfinal: hprev: {
            agda-dependencies = hprev.callCabal2nix "agda-deps" ./. { };
          }
        );
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            agda-dependencies-overlay
          ];
        };
      in
      {
        packages = {
          default = pkgs.haskellPackages.agda-dependencies;
        };

        devShells = {
          default = pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier =
              drv:
              pkgs.haskell.lib.addBuildTools drv (
                with pkgs.haskellPackages;
                [
                  cabal-install
                  haskell-language-server
                  pkgs.zlib
                ]
              );
          };
        };
      }
    );
}

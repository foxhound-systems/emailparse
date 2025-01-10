{
  description = "emailparse";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, }:
    flake-utils.lib.eachDefaultSystem ( system:
      let
        pkgs = import nixpkgs {
                    inherit system;
                    config = { allowUnfree = true; };
                    overlays = [(_final : _prev : {
                    })];
                };

        haskellPackages = pkgs.haskellPackages;

        packageName = "emailparse";

        repositoryName = "";
    in {
        packages.${packageName} =
          haskellPackages.developPackage {
              root = pkgs.lib.cleanSource ./.;
              withHoogle = false;
              modifier = pkgs.haskell.lib.dontHaddock;
          };


        packages.default = self.packages.${system}.${packageName};


        devShell = pkgs.mkShell {
            inputsFrom = [
                self.packages.${system}.default.env
            ];

            # Development packages
            buildInputs = [
              # Haskell
              pkgs.cabal-install
              haskellPackages.haskell-language-server
              haskellPackages.stylish-haskell

              # General
              pkgs.gnumake
              pkgs.entr

              pkgs.curl
              pkgs.cacert

              pkgs.ghciwatch
            ];

          };
      });
}

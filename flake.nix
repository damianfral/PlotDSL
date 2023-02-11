{
  description = "An experiment to play with the free monad and build a DSL for plotting.";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
  };

  outputs = { self, nixpkgs, flake-utils, safe-coloured-text, nix-filter, ... }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          safe-coloured-text.overlays.${system}
          nix-filter.overlays.default
        ];
      };
    in

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = pkgsFor system;
        filteredSrc =
          pkgs.nix-filter {
            root = ./.;
            include = [
              "src/"
              "test/"
              "package.yaml"
              "LICENSE"
            ];
          };
      in
      rec {
        packages = {
          plotDSL = pkgs.haskellPackages.plotDSL;
        };

        defaultPackage = packages.plotDSL;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ packages.plotDSL ];
          buildInputs = with pkgs; with pkgs.haskellPackages; [
            haskell-language-server
            cabal-install
            ghcid
            hpack
            hlint
            yamlfix
            rnix-lsp
          ];
        };

        overlays = final: prev: with final.haskell.lib; {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                plotDSL = self.callCabal2nix "plotDSL" filteredSrc { };
              }
              );
          });
        };
      });
}


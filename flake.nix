{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            thcProject =
              final.haskell-nix.cabalProject {
                index-state = "2025-02-20T00:00:00Z";
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc9101";
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  hpack = {};
                  haskell-language-server = {};
                };
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt sqlite-interactive ormolu mkdocs
                ];
                evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.thcProject.flake {};
      in flake // {
        legacyPackages = pkgs;

        packages.default = flake.packages."beam-migrate-cli:lib:beam-migrate-cli";
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}

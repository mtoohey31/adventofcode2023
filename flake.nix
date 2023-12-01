{
  description = "adventofcode2023";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    with import nixpkgs { inherit system; }; {
      devShells = {
        default = mkShell {
          name = "adventofcode";
          packages = [ ]; # stdenv includes make.
        };

        haskell = mkShell {
          name = "haskell";
          packages = [ ghc haskell-language-server hlint ormolu ];
        };
      };
    });
}

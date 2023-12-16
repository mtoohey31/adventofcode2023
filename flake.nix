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
          packages = [
            (haskellPackages.ghcWithPackages (p: [
              p.monad-memo
              p.ordered-containers
              p.relude
              p.split
              p.unordered-containers
            ]))
            haskell-language-server
            hlint
            ormolu
          ];
        };

        koka = mkShell {
          name = "koka";
          packages = [ koka ];
        };
      };
    });
}

{
  description = "Haskell Cabal";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
        triv = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;
        project-name = "lexer";
        project = devTools: let
          addDevTools = (triv.flip hl.addBuildTools) devTools;
        in
          pkgs.haskellPackages.developPackage {
            root = ./.;
            name = project-name;
            returnShellEnv = !(devTools == []);
            modifier = (triv.flip triv.pipe) [
              addDevTools
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };
      in rec {
        packages.default = project [];
        devShells.default = project (with pkgs.haskellPackages; [
          cabal-fmt
          cabal-install
          hlint
        ]);
      }
    );
}

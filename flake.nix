{
  description = "Hakyll development shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin"; # change to x86_64-linux on Linux
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        name = "hakyll-dev-shell";

        buildInputs = with pkgs; [
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.hakyll
          haskellPackages.pandoc
          pandoc
          git
          zlib
          pkg-config
        ];

        shellHook = ''
          # First-time cabal update
          if [ ! -f ".cabal-update-done" ]; then
            echo "Running 'cabal update' for the first time..."
            cabal update
            touch .cabal-update-done
          fi

          echo "Hakyll dev shell ready for ${system}"
          echo "Build the site: cabal build"
          echo "Preview site: cabal run site watch"
        '';
      };
    };
}
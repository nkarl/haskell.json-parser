{
  description = "A Nix flake for Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }:
    let

      system = "x86_64-linux";

    in
    {
      devShells."${system}".default =
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          ghc_ver = "ghc966";

        in
        pkgs.mkShell {
          packages = with pkgs; [
            cabal-install
            haskell.compiler.${ghc_ver}
            haskell-language-server
            hlint
          ];

          shellHook = ''
            echo "ghc `${pkgs.haskell.compiler.${ghc_ver}}/bin/ghc --version`"
          '';
        };
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
    };
}

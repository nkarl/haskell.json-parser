{
  description = "hsjson";

  inputs = {

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
    };
    nixpkgs = {
      follows = "haskellNix/nixpkgs-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:

    let
      
      overlays = [
        haskellNix.overlay (final: prev: {
          haskell-json =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc966";

              shell.tools = {
                cabal = {};
                hlint = {};
                stack = "3.1.1";
                haskell-language-server = {};
              };

              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];

      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

      flake = pkgs.haskell-json.flake { };

    in flake // {
      packages.default = flake.packages."hsjson:exe:hsjson";
    });
}      

#{
  #description = "haskell json";

  #inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  #};

  #outputs =  { self, nixpkgs, ... }: let

    #system = "x86_64-linux";

  #in {
    #devShells."${system}".default = let
      #pkgs = import nixpkgs {
        #inherit system;
      #};

      #packages = with pkgs; [
        #cabal-install
        #stack
        #haskell.compiler.ghc8107
        #haskell-language-server
      #];

    #in pkgs.runCommand "haskell-json" {
      #buildInputs = packages;

      #nativeBuildInputs = [ pkgs.makeWrapper ];
    #} ''
      #mkdir -p $out/bin/
      #ln -s ${pkgs.ghc}/bin/ghc $out/bin/ghc
      #wrapProgram $out/bin/ghc --prefix PATH : ${pkgs.lib.makeBinPath packages}
    #'';
  #};
#}

{
  description = "baka";

  inputs = {
    nixpkgs      = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    flake-utils  = { url = "github:numtide/flake-utils"; };
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };

  outputs = { self, nixpkgs, flake-compat, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        name = "baka";
        ghc = pkgs.haskell.compiler.ghc912;
        pkgs = nixpkgs.legacyPackages.${system};
        revision = "${self.lastModifiedDate}-${self.shortRev or "dirty"}";
      in rec {
        # nix develop
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cacert
            git
            gmp
            cabal-install
            ghc
            hlint
            sourceHighlight
            watchexec
            zlib.dev
          ];
          shellHook = ''
            export LANG=en_US.UTF-8
            export PS1="baka|$PS1"
          '';
        };
      }
    );
}

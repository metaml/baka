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
        pname = "baka";
        ghc-version = "ghc981";
        pkgs = nixpkgs.legacyPackages.${system};
        hkgs = pkgs.haskell.packages.${ghc-version}; # "make update" first sometimes helps

        baka = pkgs.runCommand
          "baka"
          { preferLocalBuild = true;  buildInputs = [ pname ]; }
          "";

        revision = "${self.lastModifiedDate}-${self.shortRev or "dirty"}";
      in rec {
        # nix develop
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cacert
            git
            gmp
            hkgs.cabal-install
            hkgs.ghc
            hkgs.haskell-language-server
            hkgs.hlint
            pcre.dev
            sourceHighlight
            watchexec
            zlib.dev
          ];
          shellHook = ''
            export LANG=en_US.UTF-8
            export GOOGLE_PROJECT=lpgprj-gss-p-ctrlog-gl-01
            export GOOGLE_REGION=us-east1
            export GOOGLE_ZONE=us-east1-c
            export PS1="babel|$PS1"
          '';
        };
      }
    );
}

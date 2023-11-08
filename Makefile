.DEFAULT_GOAL = build

export SHELL := /bin/sh
export NIX_REMOTE = daemon
export GOOGLE_APPLICATION_CREDENTIALS ?= tmp/lpgprj-gss-p-ctrlog-gl-01-c0096aaa9469.json

PROJECT ?= ${HOME}/${LOGNAME}/p/baka
PWD     := $(shell pwd)

BIN ?= baka

CABAL_BUILD = cabal build --jobs \$$ncpus --minimize-conflict-set 

build: clean ## build (default)
	@$(CABAL_BUILD) 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

buildc: clean ## build continuously
	@$(CABAL_BUILD) 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc
	watchexec --exts cabal,hs -- $(CABAL_BUILD) 2>&1 \
	| source-highlight --src-lang=haskell --out-format=esc

install: # install binary
	rm -rf bin/* && mkdir -p bin
	cabal build --verbose
	cabal install --overwrite-policy=always --install-method=copy --installdir=bin

dev: ## nix develop
	nix develop

test: ## test
	cabal $(OPT) test

lint: ## lint
	hlint app src

clean: ## clean
	-cabal clean

clobber: clean ## cleanpq
	rm -rf dist-newstyle
	rm -rf tmp/*

# make activate KEY_FILE=... first
run: export GOOGLE_APPLICATION_CREDENTIALS ?= tmp/lpgprj-gss-p-ctrlog-gl-01-c0096aaa9469.json
run: ## run BIN, e.g. make run BIN=<binary>
	cabal run $(BIN) -- $(ARG)

#repl: export GOOGLE_APPLICATION_CREDENTIALS ?= /Users/milee/.zulu/lpgprj-gss-p-ctrlog-gl-01-c0096aaa9469.json
repl: ## repl
	cabal repl

update: ## update nix and cabal project dependencies
update: nix-update-all cabal-update

cabal-update: ## update cabal project depedencies
	nix develop \
	&& cabal update \
	&& exit

flake-update: ## update nix and project dependencies
	nix flake update

help: ## help
	-@grep --extended-regexp '^[0-9A-Za-z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	  | sed 's/^Makefile://1' \
	  | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	-@ghc --version
	-@cabal --version
	-@hlint --version

nix-build: ## build with nix
	nix --print-build-logs build --impure

nix-install: ## install to profile
	nix profile install

nix-clean: ## clean up /nix
	nix-collect-garbage --delete-old

nix-clobber: ## clean up everything: https://nixos.org/guides/nix-pills/garbage-collector.html
	sudo rm -f /nix/var/nix/gcroots/auto/*
	nix-collect-garbage --delete-old

nix-update-all: ## init/update nix globally
	nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
	nix-channel --update
	sudo nix profile upgrade '.*'
	nix flake update


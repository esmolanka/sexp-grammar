{compiler ? "ghc865" }:
let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz";
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }) { inherit config; };

  mkRelative = root: path:
    nixpkgs.lib.removePrefix (toString root + "/") path;

  gitignored = nixpkgs.nix-gitignore.gitignoreSourcePure
    [ ./.gitignore ];

  gitignoredRegex = regex: root:
    nixpkgs.nix-gitignore.gitignoreFilterSourcePure
      (path: type: builtins.match regex (mkRelative root path) != null)
      [ ./.gitignore ] root;

  mkCabalPackage = self: srcFilter: name: originalSource:
    nixpkgs.haskell.lib.overrideCabal
      (self.callCabal2nix name (srcFilter originalSource) {})
      (old: rec {
        doHaddock = false;
        doHoogle = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      });

  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${compiler}.override {
        overrides = new: old: {
          invertible-grammar = mkCabalPackage new gitignored "invertible-grammar" ./invertible-grammar;
          sexp-grammar = mkCabalPackage new gitignored "sexp-grammar" ./sexp-grammar;
        };
      };
    };
  };
in

nixpkgs

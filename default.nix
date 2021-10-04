{ compiler ? "ghc901" }:
let
  # nixpkgs = import (builtins.fetchTarball {
  #   url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz";
  #   sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
  # }) { inherit config; };

  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/d432fcbf37034ba102197d6e0756e972600bdbe2.tar.gz";
    sha256 = "0lj809bc9y5xqc52kcrbcm6dc6nfa9icmw20r1d4hk51jp6550lf";
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
        doCheck = true;
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

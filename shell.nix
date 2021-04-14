{ compiler ? "ghc884" }:

let
  nixpkgs = import ./. { inherit compiler; };
in

nixpkgs.haskellPackages.shellFor {
  packages = p: with p; [
    sexp-grammar
    invertible-grammar
  ];
  buildInputs = [
    nixpkgs.haskellPackages.cabal-install
  ];
}

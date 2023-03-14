{ compiler ? "ghc925" }:
let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
    sha256 = "11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
  }) { };
in

nixpkgs.mkShell {
  buildInputs = [
    nixpkgs.haskell.packages.${compiler}.cabal-install
    (nixpkgs.haskell.packages.${compiler}.ghcWithPackages (p: [ ]))
  ];
}

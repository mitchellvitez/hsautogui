{ pkgs ? import ./nix/pkgs.nix {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc884
    pkgs.python39
    pkgs.cabal-install
    pkgs.cabal2nix
  ];
}

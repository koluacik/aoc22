{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    haskell-language-server
    cabal-install
    ormolu
    (haskellPackages.ghcWithHoogle (hp: with hp; [
    ]))
  ];
}

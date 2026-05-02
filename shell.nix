{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc96;
  myProject = haskellPackages.callCabal2nix "hcc" ./. {};
in
pkgs.mkShell {
  inputsFrom = [ myProject.env ];
  buildInputs = with pkgs; [
    cabal-install
  ];
  shellHook = ''
    exec zsh
  '';
}

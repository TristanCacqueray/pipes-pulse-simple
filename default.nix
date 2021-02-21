{ nixpkgs ? import <nixpkgs> { }, withHoogle ? false }:
let
  name = "pipes-pulse-simple";
  drv = nixpkgs.haskellPackages.callCabal2nix name ./. { };
  shellDrv = nixpkgs.haskellPackages.shellFor {
    withHoogle = withHoogle;
    packages = p: [ drv ];
    buildInputs = with nixpkgs.haskellPackages; [
      haskell-language-server
      hlint
      cabal-install
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv

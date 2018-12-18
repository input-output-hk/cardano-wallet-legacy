{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; application = "cardano-sl"; }
, pkgs ? iohkLib.pkgs
, gitrev ? iohkLib.commitIdFromGitRepo ./.
}:

let
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs;
    src = iohkLib.cleanSourceHaskell ./.;
  };

in {
  inherit haskellPackages;

  inherit (haskellPackages.cardano-wallet.components)
    benchmarks exes library tests;
}

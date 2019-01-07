############################################################################
# Cardano Wallet Nix build
#
# To build the wallet, run:
#
#    nix-build -A exes.cardano-node
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}

# Import IOHK common nix lib and pinned nixpkgs
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; application = "cardano-sl"; }
, pkgs ? iohkLib.pkgs

# Keep this argument even if unused.
# It will prevent Hydra from caching the evaluation.
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

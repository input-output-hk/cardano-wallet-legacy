############################################################################
# Hydra release jobset
#
# Example build for Linux:
#
#   nix-build release.nix -A exes.cardano-node.x86_64-linux
#
# Example build for Windows (cross-compiled from Linux):
#
#   nix-build release.nix -A cross.exes.cardano-node.x86_64-linux
#
############################################################################

let
  iohkLib = import ./nix/iohk-common.nix { application = "cardano-sl"; };
  fixedNixpkgs = iohkLib.pkgs;

in { supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
  , scrubJobs ? true
  , cardano-wallet ? { outPath = ./.; rev = "abcdef"; }
  , nixpkgsArgs ? {
      config = (import ./nix/config.nix // { allowUnfree = false; inHydra = true; });
      gitrev = cardano-wallet.rev;
    }
  }:

with (import (fixedNixpkgs.path + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import cardano-wallet.outPath;
});

let
  jobs = mapTestOn {
    exes.cardano-node = supportedSystems;
    tests.unit        = supportedSystems;
  };

  crossJobs = mapTestOnCross lib.systems.examples.mingwW64 {
    exes.cardano-node = [ "x86_64-linux" ];
    tests.unit        = [ "x86_64-linux" ];
  };

in
  jobs // { cross = crossJobs; }

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

  cardano-sl-src = haskellPackages._config.packages.cardano-sl.src;

  cardano-sl-config = pkgs.runCommand "cardano-sl-config" {} ''
    mkdir -p $out/lib
    cp ${cardano-sl-src}/lib/configuration.yaml $out/lib
    cp ${cardano-sl-src}/lib/*.json $out/lib
    cp -R ${cardano-sl-src}/log-configs $out
  '';

in {
  inherit haskellPackages;

  inherit (haskellPackages.cardano-wallet.components)
    benchmarks exes library;

  tests = haskellPackages.cardano-wallet.components.tests // (let
    acceptanceTestDeps = {
      inherit (haskellPackages.cardano-wallet.components.exes)
        cardano-wallet-server
        cardano-wallet-client
        cardano-wallet-acceptance-tests
        cardano-wallet-connect-script
        cardano-wallet-sync-plot;
      inherit (haskellPackages.cardano-sl-tools.components.exes)
        cardano-x509-certificates;
      inherit cardano-sl-config;
    };
  in {
    acceptance = pkgs.callPackage ./nix/acceptance-tests acceptanceTestDeps;
    acceptance-windows = pkgs.callPackage ./nix/acceptance-tests/windows.nix acceptanceTestDeps;
  });
}

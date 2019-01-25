{ lib, stdenv, writeScript
, jq, coreutils, curl, gnused, openssl, time, haskellPackages

, cardano-wallet-server, cardano-wallet-client
, cardano-wallet-acceptance-tests, cardano-wallet-connect-script
, cardano-x509-certificates
, cardano-wallet-sync-plot

, cardano-sl-config

, environment ? "mainnet"
}:

let
  cardanoDeps = [
    cardano-wallet-server cardano-wallet-client
    cardano-wallet-acceptance-tests cardano-wallet-connect-script
    cardano-x509-certificates
    # cardano-wallet-sync-plot
  ];
  testRunnerDeps = [ jq coreutils curl gnused openssl time haskellPackages.hp2pretty ];
  allDeps =  testRunnerDeps ++ cardanoDeps;

in
  writeScript "acceptance-tests-${environment}" ''
    #!${stdenv.shell}

    export PATH=${lib.makeBinPath allDeps}:$PATH

    cp -R --no-preserve=mode ${cardano-sl-config}/* .

    command time -v cardano-wallet-acceptance-tests ${environment}

    hp2pretty cardano-node.hp
    # cardano-wallet-sync-plot sync sync-stats.json sync-stats.png
    # cardano-wallet-sync-plot restore restore-stats.json restore-stats.png

    if [ -n "$BUILDKITE" ]; then
      buildkite-agent artifact upload sync-stats.png
      buildkite-agent artifact upload restore-stats.png
      buildkite-agent artifact upload cardano-node.hp
      buildkite-agent artifact upload cardano-node.svg
      echo "+++ Heap profile"
      printf '\033]1338;url='"artifact://cardano-wallet-server.svg"';alt='"Heap profile"'\a\n'
      echo "+++ Charts for ${environment}"
      printf '\033]1338;url='"artifact://sync-stats.png"';alt='"Blockchain sync"'\a\n'
      printf '\033]1338;url='"artifact://restore-stats.png"';alt='"Wallet restore"'\a\n'
    fi
  ''

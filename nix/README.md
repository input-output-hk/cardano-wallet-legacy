## Nix build of cardano-wallet

See build instructions in `../default.nix` and `../release.nix`.

If the Stack or Cabal dependencies change, the `stack-to-nix`
generated code must be regenerated. To do that, run:

    ./regenerate.sh

The Buildkite pipeline checks that the generated code is up to date.

# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ./.. { inherit config system; }
, pkgs ? iohkPkgs.pkgs
}:
with pkgs;

haskell.lib.buildStackProject {
  name = "cardano-wallet-stack-env";
  ghc = iohkPkgs.haskellPackages._config.ghc.package;

  buildInputs = [
    zlib gmp rocksdb ncurses lzma
  ] ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}

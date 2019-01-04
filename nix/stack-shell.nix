############################################################################
# Nix environment referred to from stack.yaml.
#
# If the GHC version or system dependencies change, then update this file.
#
# This is using an unpinned nixpkgs until nix-tools is merged.
#
############################################################################

with import <nixpkgs> {};

haskell.lib.buildStackProject {
  inherit (haskell.packages.ghc844) ghc;
  name = "cardano-wallet-stack-env";

  buildInputs = [
    autoreconfHook perl bash git
    openssl gmp rocksdb bsdiff ncurses lzma cairo
  ] ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}

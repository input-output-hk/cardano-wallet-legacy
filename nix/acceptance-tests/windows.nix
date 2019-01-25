{ lib, stdenv, runCommand, writeText, fetchurl, zip, unzip

, cardano-wallet-server, cardano-wallet-client
, cardano-wallet-acceptance-tests, cardano-wallet-connect-script
, cardano-x509-certificates, cardano-wallet-sync-plot

, cardano-sl-config
}:

let
  deps = [
    cardano-wallet-server cardano-wallet-client
    cardano-wallet-acceptance-tests cardano-wallet-connect-script
    cardano-x509-certificates
  ];

  # These are from the Daedalus installer build script.
  dlls = fetchurl {
    url = https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip;
    sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
  };

  name = "acceptance-tests";

  driver = writeText "run.bat" ''
    cardano-wallet-acceptance-tests mainnet
  '';

in
  runCommand name {} ''
    mkdir -p $out tmp

    cd tmp
    cp ${driver} run.bat
    cp -Rv --no-preserve=mode ${cardano-sl-config}/* .
    ${lib.concatMapStringsSep "\n" (dep: "cp -v ${dep}/bin/* .") deps}

    ${unzip}/bin/unzip ${dlls}

    ${zip}/bin/zip -r $out/${name}.zip *

    mkdir -p $out/nix-support
    echo "file binary-dist \"$out/${name}.zip\"" \
        > $out/nix-support/hydra-build-products
  ''

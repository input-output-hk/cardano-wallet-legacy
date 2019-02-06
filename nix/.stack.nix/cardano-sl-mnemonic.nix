{ system
, compiler
, flags
, pkgs
, hsPkgs
, pkgconfPkgs
, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-sl-mnemonic";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/mnemonic/README.md";
      url = "";
      synopsis = "TODO";
      description = "See README";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.basement)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.swagger2)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.universum)
        ];
      };
      exes = {
        "cardano-generate-mnemonic" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.cardano-sl-mnemonic)
            (hsPkgs.bytestring)
            (hsPkgs.text)
            (hsPkgs.universum)
          ];
        };
      };
      tests = {
        "cardano-sl-mnemonic-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.hspec)
            (hsPkgs.universum)
            (hsPkgs.cardano-sl-mnemonic)
            (hsPkgs.bytestring)
            (hsPkgs.QuickCheck)
            (hsPkgs.cardano-sl-crypto)
            (hsPkgs.data-default)
            (hsPkgs.aeson)
            (hsPkgs.cardano-crypto)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "632769d4480d3b19299d801c9fb39e75d20dd7d9";
      sha256 = "1l9i62fdgcl2spgaag70bxnm2rz996bl6g5nhmhj5m5fwn4sy2b9";
    };
    postUnpack = "sourceRoot+=/mnemonic; echo source root reset to \$sourceRoot";
  }
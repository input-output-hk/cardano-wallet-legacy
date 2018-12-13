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
        name = "cardano-sl-core-test";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "IOHK <support@iohk.io>";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Cardano SL - core functionality (tests)";
      description = "QuickCheck Arbitrary instances for the Cardano SL core\nfunctionality.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.QuickCheck)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-sl-binary)
          (hsPkgs.cardano-sl-binary-test)
          (hsPkgs.cardano-sl-core)
          (hsPkgs.cardano-sl-crypto)
          (hsPkgs.cardano-sl-crypto-test)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.cardano-sl-util-test)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.generic-arbitrary)
          (hsPkgs.hedgehog)
          (hsPkgs.quickcheck-instances)
          (hsPkgs.random)
          (hsPkgs.serokell-util)
          (hsPkgs.text)
          (hsPkgs.time-units)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "f39f9b033e615af55ba352b7034d96ebb068e61f";
      sha256 = "1v6384iy2kjp367qs6jnqm7lsa55wgh69z6dqf70vysqn9bc2iab";
    };
    postUnpack = "sourceRoot+=/core/test; echo source root reset to \$sourceRoot";
  }
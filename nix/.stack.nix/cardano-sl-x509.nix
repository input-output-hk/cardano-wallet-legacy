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
        name = "cardano-sl-x509";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-sl/x509/README.md";
      url = "";
      synopsis = "Tool-suite for generating x509 certificates specialized for RSA with SHA-256";
      description = "See README";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.asn1-encoding)
          (hsPkgs.asn1-types)
          (hsPkgs.base64-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default-class)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.hourglass)
          (hsPkgs.ip)
          (hsPkgs.universum)
          (hsPkgs.unordered-containers)
          (hsPkgs.x509)
          (hsPkgs.x509-store)
          (hsPkgs.x509-validation)
          (hsPkgs.yaml)
        ];
      };
      tests = {
        "cardano-sl-x509-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.QuickCheck)
            (hsPkgs.cardano-sl-x509)
            (hsPkgs.universum)
          ];
        };
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "f39f9b033e615af55ba352b7034d96ebb068e61f";
      sha256 = "1v6384iy2kjp367qs6jnqm7lsa55wgh69z6dqf70vysqn9bc2iab";
    };
    postUnpack = "sourceRoot+=/x509; echo source root reset to \$sourceRoot";
  }
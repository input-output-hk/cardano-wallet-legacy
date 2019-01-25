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
        name = "cardano-sl-node-ipc";
        version = "2.0.0";
      };
      license = "MIT";
      copyright = "";
      maintainer = "cleverca22@gmail.com";
      author = "Michael Bishop";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-sl-infra)
          (hsPkgs.cardano-sl-util)
          (hsPkgs.Cabal)
          (hsPkgs.mtl)
          (hsPkgs.universum)
        ];
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "cdc77284bde949d4b1b9ac7be81b76b906a71fc7";
      sha256 = "1hz27007z40w7mba6hfpf3jpmh9xdbf2cnmdmiskic2msvh0rdy7";
    };
    postUnpack = "sourceRoot+=/node-ipc; echo source root reset to \$sourceRoot";
  }
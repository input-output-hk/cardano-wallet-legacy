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
        name = "acid-state-exts";
        version = "0.14.2";
      };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "Lemmih <lemmih@gmail.com>";
      author = "David Himmelstrup";
      homepage = "https://github.com/acid-state/acid-state";
      url = "";
      synopsis = "Add ACID guarantees to any serializable Haskell data structure.";
      description = "Use regular Haskell data structures as your database and get stronger ACID guarantees than most RDBMS offer.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs.acid-state)
          (hsPkgs.base)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.exceptions)
          (hsPkgs.extra)
          (hsPkgs.mtl)
          (hsPkgs.safecopy)
          (hsPkgs.time-units)
        ] ++ pkgs.lib.optional (system.isWindows) (hsPkgs.Win32);
      };
    };
  } // {
    src = pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-sl";
      rev = "f39f9b033e615af55ba352b7034d96ebb068e61f";
      sha256 = "1v6384iy2kjp367qs6jnqm7lsa55wgh69z6dqf70vysqn9bc2iab";
    };
    postUnpack = "sourceRoot+=/acid-state-exts; echo source root reset to \$sourceRoot";
  }
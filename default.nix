{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; application = "cardano-sl"; }
, pkgs ? iohkLib.pkgs
, gitrev ? iohkLib.commitIdFromGitRepo ./.
}:

import ./nix/pkgs.nix { inherit pkgs; }

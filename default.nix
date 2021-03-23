# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs ? import <nixpkgs> {}
, src ? ./.
, version ? "git"
, ... }:

let
  nix-lisp = import (pkgs.fetchFromGitHub {
    owner = "nix-lisp";
    repo = "lisp-overlay";
    rev = "99c738ad1c309b8fde6c95d62b7dab0092d0f23b";
    sha256 = "1rb5yygsyzzc6byajzyf78dp86p9k2igqpsk561k6276mgkrfijw";
    # date = 2020-12-08T04:57:09+00:00;
  }) {};
in pkgs.nyxt.overrideAttrs (drv: {
  src = drv.src.overrideAttrs (drv: {
    inherit src;
    name = pkgs.lib.replaceStrings [drv.meta.version] [version] drv.name;

    # Pending nixpkgs changes
    installPhase = builtins.replaceStrings ["nyxt-ext"] ["nyxt"] drv.installPhase;

    # Package polyfills
    propagatedBuildInputs = drv.propagatedBuildInputs ++ [
      nix-lisp.calispel
    ];
  });
  inherit version;
})

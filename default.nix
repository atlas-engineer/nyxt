# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

{ pkgs ? import <nixpkgs> {}
, src ? ./.
, version ? "git"
, ... }:

pkgs.nyxt.overrideAttrs (drv: {
  src = drv.src.overrideAttrs (drv: {
    inherit src;
    name = pkgs.lib.replaceStrings [drv.meta.version] [version] drv.name;
  });
  inherit version;
})

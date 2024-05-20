# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# To start the CL REPL:

# nix-shell /path/to/shell.nix --run 'sbcl --dynamic-space-size 3072'

{ pkgs ? import <nixpkgs> {} } :
with builtins;
let inherit (pkgs) stdenv; in
with pkgs;
stdenv.mkDerivation {
  name = "nyxt-dev";

  nativeBuildInputs = [
    pkgs.libressl.out
    pkgs.libfixposix.out
    pkgs.sqlite.out
    pkgs.pkg-config.out
    pkgs.sbcl
  ]
  ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    pkgs.webkitgtk
  ];

  buildInputs = [
    pkgs.sqlite
    pkgs.pkg-config
    pkgs.enchant.out
    pkgs.libfixposix.out
  ]
  ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    pkgs.gobject-introspection
    pkgs.gsettings-desktop-schemas.out
    pkgs.glib-networking.out
    pkgs.pango.out
    pkgs.cairo.out
    pkgs.gdk-pixbuf.out
    pkgs.gtk3.out
    pkgs.glib.out
    pkgs.webkitgtk
  ]
  ++ (with gst_all_1; [
    gst-plugins-base
    gst-plugins-good
    gst-plugins-bad
    gst-plugins-ugly
    gst-libav
  ]);

  LD_LIBRARY_PATH = with lib;
    makeLibraryPath (
      [
        pkgs.enchant.out
        pkgs.sqlite.out
        pkgs.pkg-config.out
        pkgs.libfixposix.out
        pkgs.libressl.out
      ]
      ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
        pkgs.gobject-introspection
        pkgs.gsettings-desktop-schemas.out
        pkgs.glib-networking.out
        pkgs.pango.out
        pkgs.cairo.out
        pkgs.gdk-pixbuf.out
        pkgs.gtk3.out
        pkgs.glib.out
        pkgs.webkitgtk
      ]
    );

  GIO_MODULE_DIR = "${pkgs.glib-networking.out}/lib/gio/modules/";
  GIO_EXTRA_MODULES = "${pkgs.glib-networking.out}/lib/gio/modules/";

  shellHook =
    ''
    NYXT_ROOT=`dirname ${toString ../README.org}`;
    export CL_SOURCE_REGISTRY=$HOME/common-lisp//:$NYXT_ROOT/_build//:$CL_SOURCE_REGISTRY;
    '';

}

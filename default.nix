{ pkgs ? import <nixpkgs> {}
, src ? ./.
, version ? "nix"
, ... }:

pkgs.nyxt.overrideAttrs (drv: {
  src = drv.src.overrideAttrs (drv: {
    inherit src;
    name = pkgs.lib.replaceStrings [drv.meta.version] [version] drv.name;

    # Pending nixpkgs changes
    installPhase = builtins.replaceStrings ["nyxt-ext"] ["nyxt"] drv.installPhase;
  });
  inherit version;
})

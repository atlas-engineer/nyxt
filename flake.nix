{
  description = "Nyxt flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  }; # Not locked

  outputs = inputs@{ self, nixpkgs, ... }: let

    systems = builtins.attrNames nixpkgs.legacyPackages;

    mkNyxt = { pkgs, ... }: import ./. {
      inherit pkgs;
      src = inputs.self;
      version = inputs.self.lastModifiedDate;
    };

  in {
  
    packages = nixpkgs.lib.genAttrs systems (system: {
      nyxt = mkNyxt {
        pkgs = nixpkgs.legacyPackages.${system};
      };
    });

    defaultPackage = nixpkgs.lib.genAttrs systems (system: 
      self.packages.${system}.nyxt
    );

    overlays = {
      nyxt = final: prev: {
        nyxt = mkNyxt { pkgs = prev; };
      };
    };

    overlay = self.overlays.nyxt;

    apps = nixpkgs.lib.genAttrs systems (system: {
      nyxt = rec {
        type = "app";
        program = "${out}/bin/nyxt";
        out = self.apps.${system}.nyxt;
      };
    });

    defaultApp = nixpkgs.lib.genAttrs systems (system: 
      self.apps.${system}.nyxt
    );

  };

}

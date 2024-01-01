{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";

    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ nixpkgs, ...}:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      # systems = [ "x86_64-linux" ];
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        haskellProjects.default = {
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`, e.g. from Hackage
          packages = {
            # aeson.source = "1.5.0.0" # Hackage version
            # taggy-lens.source = inputs.taggy-lens;
          };

          # my-haskell-package development shell configuration
          devShell = {
            hlsCheck.enable = true;
            hoogle = false;
            tools = hp: {
              inherit (hp)
                fourmolu
                rio
                text-builder
                ;
              # attoparsec
              # base-unicode-symbols
              # formatting
              # lens
              # modern-uri
              # optparse-simple
              # pathtype
              # pretty-simple
              # purebred-email
              # replace-attoparsec
              # string-conversions
            };
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" ]; # Wire all but the devShell
        };

        devShells.default = pkgs.mkShell {
          name = "haskell dev";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            # other development tools.
            (import ./vim.nix { inherit pkgs; })
          ];
        };
      };
    };
}

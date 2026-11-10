{
  description = "umbriel.el: Emacs client for the umbriel Wayland compositor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Wrapped Emacs (has magit-section). Own pins, no follows, so every
    # devshell shares one build.
    emacs.url = "github:davidlee/nix-config?dir=flakes/emacs";
  };

  outputs = {
    nixpkgs,
    emacs,
    ...
  }: let
    systems = ["x86_64-linux" "aarch64-darwin"];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system nixpkgs.legacyPackages.${system});
  in {
    devShells = forAllSystems (system: pkgs: {
      default = pkgs.mkShell {
        packages = [
          emacs.packages.${system}.default
          pkgs.just
          pkgs.ripgrep
        ];
      };
    });
  };
}

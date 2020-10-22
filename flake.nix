{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
    hardware.url = "github:NixOS/nixos-hardware";

    # TODO: Wait for 20.09 to integrate home-manager
    home-manager.url = "github:nix-community/home-manager";

    mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs =
    { self, nixpkgs, hardware, home-manager, mozilla, emacs }:
    {
      nixosConfigurations = {
        alpha128 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/alpha128/configuration.nix
            nixpkgs.nixosModules.notDetected
          ];
        };

        alpha32 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/alpha32/configuration.nix
            hardware.nixosModules.lenovo-thinkpad-e495
            nixpkgs.nixosModules.notDetected
          ];
        };

        gamma64 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/gamma64/configuration.nix
            nixpkgs.nixosModules.notDetected
          ];
        };
      };
    };
}

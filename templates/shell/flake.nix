{
  description = "A basic shell flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    utils,
  }: let
    mkShell = pkgs: pkgs.mkShell {buildInputs = with pkgs; [];};
  in
    utils.lib.eachDefaultSystem
    (
      system: let
        pkgs = import nixpkgs {inherit system;};
      in {
        legacyPackages = pkgs;
        devShell = mkShell pkgs;
      }
    );
}

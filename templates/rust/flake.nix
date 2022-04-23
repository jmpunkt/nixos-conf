{
  description = "A rust project";
  inputs = {
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    utils,
    rust-overlay,
  }: let
    buildShell = pkgs: anyRustToolchain: pkgs.mkShell {buildInputs = with pkgs; [anyRustToolchain];};
    rustOverwrite = anyRustToolchain: anyRustToolchain.override {extensions = ["rust-src" "rust-analyzer-preview"];};
    buildForSystem = system: let
      overlays = [rust-overlay.overlay];
      pkgs = import nixpkgs {inherit system overlays;};
    in {
      devShells = {
        stable = buildShell pkgs (rustOverwrite pkgs.rust-bin.stable.latest.default);
        nightly = buildShell pkgs (rustOverwrite pkgs.rust-bin.nightly.latest.default);
        beta = buildShell pkgs (rustOverwrite pkgs.rust-bin.beta.latest.default);
      };
      devShell = self.devShells.${system}.stable;
      legacyPackages = pkgs;
    };
  in (utils.lib.eachDefaultSystem buildForSystem);
}

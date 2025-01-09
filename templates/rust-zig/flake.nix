{
  description = "A rust project with cargo-zigbuild";
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
      overlays = [rust-overlay.overlays.default];
      pkgs = import nixpkgs {inherit system overlays;};
      pkgsCross = import nixpkgs {
        inherit system overlays;
        crossSystem = {
          config = "armv7a-unknown-linux-musleabihf";
          rustc.config = "armv7-unknown-linux-musleabihf";
        };
      };
      rustPlatform = let
        rustBundle = (pkgs.rust-bin.stable.latest.minimal).override {
          targets = ["armv7-unknown-linux-musleabihf"];
        };
      in
        pkgs.makeRustPlatform {
          cargo = rustBundle;
          rustc = rustBundle;
        };
    in {
      devShells = rec {
        default = nightly;
        stable = buildShell pkgs (rustOverwrite pkgs.rust-bin.stable.latest.default);
        nightly = buildShell pkgs (rustOverwrite pkgs.rust-bin.nightly.latest.default);
        beta = buildShell pkgs (rustOverwrite pkgs.rust-bin.beta.latest.default);
      };
      legacyPackages = pkgs;
      packages.example-project = ((rustPlatform.buildRustPackage).override {stdenv = pkgsCross.stdenvNoCC;}) {
        name = "example-project";
        src = ./.;
        cargoLock = {
          lockFile = ./Cargo.lock;
        };
        preBuild = ''
          alias cargo="${pkgs.cargo-zigbuild}/bin/cargo-zigbuild"
        '';
      };
    };
  in (utils.lib.eachDefaultSystem buildForSystem);
}

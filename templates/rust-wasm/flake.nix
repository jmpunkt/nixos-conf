{
  description = "A rust wasm project";
  inputs = {
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      utils,
      rust-overlay,
    }:
    let
      buildShell =
        pkgs: anyRustToolchain:
        pkgs.mkShell {
          buildInputs = with pkgs; [
            anyRustToolchain
            cargo-zigbuild
            zig
          ];
        };
      rustOverwrite =
        anyRustToolchain:
        anyRustToolchain.override {
          extensions = [
            "rust-src"
            "rust-analyzer-preview"
          ];
          targets = [
            "wasm32-unknown-unknown"
            "wasm32-wasip1"
          ];
        };
      buildForSystem =
        system:
        let
          overlays = [ rust-overlay.overlays.default ];
          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          devShells = rec {
            default = nightly;
            stable = buildShell pkgs (rustOverwrite pkgs.rust-bin.stable.latest.default);
            nightly = buildShell pkgs (rustOverwrite pkgs.rust-bin.nightly.latest.default);
            beta = buildShell pkgs (rustOverwrite pkgs.rust-bin.beta.latest.default);
          };
          legacyPackages = pkgs;
        };
    in
    (utils.lib.eachDefaultSystem buildForSystem);
}

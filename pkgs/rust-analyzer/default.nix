# Taken from https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/rust/rust-analyzer/generic.nix (3ea54e69727b0195f25a2be909ae821223621a64)

{ lib, mozillaRustPlatform, stdenv, fetchFromGitHub, darwin }:

# Requires newest version of Rust (most of the time)
mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer-unwrapped";
  version = "2020-06-29";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "1b3zswar8kahagi952x9yk2aynwxf1plfs1kfw5r9gy7y8s174y9";
  };

  cargoSha256 = "1bdkg19833ic0ks9j4rpfjlqw3hv37v25laqqgcvz369hlw7sh1p";

  preBuild = "pushd crates/rust-analyzer";
  # Do not checking other crates in checkPhase.
  preInstall = "popd";

  buildInputs = lib.optionals stdenv.hostPlatform.isDarwin
    [ darwin.apple_sdk.frameworks.CoreServices ];

  # Skip tests running `rustup` for `cargo fmt`.
  preCheck = ''
    fakeRustup=$(mktemp -d)
    ln -s $(command -v true) $fakeRustup/rustup
    export PATH=$PATH''${PATH:+:}$fakeRustup
  '';

  meta = with stdenv.lib; {
    description =
      "An experimental modular compiler frontend for the Rust language";
    homepage = "https://github.com/rust-analyzer/rust-analyzer";
    license = with licenses; [ mit asl20 ];
    platforms = platforms.all;
  };
}

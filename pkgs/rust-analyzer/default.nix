# Taken from https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/rust/rust-analyzer/generic.nix (3ea54e69727b0195f25a2be909ae821223621a64)

{ lib, mozillaRustPlatform, stdenv, fetchFromGitHub, darwin }:

# Requires newest version of Rust (most of the time)
mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer-unwrapped";
  version = "2020-07-20";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "0bp6byaq9g3y2nxpflyj703q3bjfr2lf0wlb1b2kkjvzfnyygj5w";
  };

  cargoSha256 = "1llwankjm7ay8gk1dl1c8w4jj18qx2yx8ayb39an7pzncwfqrah7";

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

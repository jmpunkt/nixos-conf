# Taken from https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/tools/rust/rust-analyzer/generic.nix (3ea54e69727b0195f25a2be909ae821223621a64)

{ lib, mozillaRustPlatform, stdenv, fetchFromGitHub, darwin }:

mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer-unwrapped";
  version = "2020-06-01";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "0chm47mrd4hybhvzn4cndq2ck0mj948mm181p1i1j1w0ms7zk1fg";
  };

  cargoSha256 = "1jr6y80m99w47vklfh0izs9zn1vrxmsziwmdrhcxvi20hg6pv0q4";

  preBuild = "pushd crates/rust-analyzer";
  # Do not checking other crates in checkPhase.
  preInstall = "popd";

  # nativeBuildInputs = [ mozillaRustPlatform.rust-src ];

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

{ stdenv, mozillaRustPlatform, fetchFromGitHub, makeWrapper }:

mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer";
  version = "2020-03-30";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "18jcv3j255p5vqhvy6ph6a8hrpk0ljilh8dpgxpbh4w7cmsxgnig";
  };

  nativeBuildInputs = [ makeWrapper ];

  # Remove usage of rustup for the `codegen` path.
  patches = [ ./remove-rustup.patch ];

  # Exclude xtask since it depends on rustup
  cargoBuildFlags = [ "--bin rust-analyzer" ];

  preBuild = ''
    cargo xtask codegen
  '';

  postInstall = ''
    wrapProgram $out/bin/rust-analyzer --set "PATH" "$PATH:${mozillaRustPlatform.rust.cargo}/bin/"
  '';

  cargoSha256 = "08gm3a5l3yx6l7dyaamnvzwjgby8wfpx1q65sq3xzh1dq93kp8kr";

  meta = with stdenv.lib; {
    description = "An experimental Rust compiler front-end for IDEs";
    homepage = "https://github.com/rust-analyzer/rust-analyzer";
    license = [ licenses.mit licenses.asl20 ];
    maintainers = [ ];
  };
}

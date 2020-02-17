{ stdenv, mozillaRustPlatform, fetchFromGitHub, makeWrapper }:

mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer";
  version = "2020-02-17";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "04bl9k7sv51hb5r0lcxb5sa6y3v7ia1xay79dchy43dg3lm0yvvs";
  };

  nativeBuildInputs = [ makeWrapper ];

  # Remove usage of rustup for the `codegen` path.
  patches = [ ./remove-rustup.patch ];

  # Exclude xtask since it depends on rustup
  cargoBuildFlags = [ "--bin ra_cli" "--bin ra_lsp_server" ];

  preBuild = ''
    cargo xtask codegen
  '';

  postInstall = ''
    wrapProgram $out/bin/ra_cli --set "PATH" "$PATH:${mozillaRustPlatform.rust.cargo}/bin/"
    wrapProgram $out/bin/ra_lsp_server --set "PATH" "$PATH:${mozillaRustPlatform.rust.cargo}/bin/"
  '';

  cargoSha256 = "0ha71kv54aijaadcakpb4hr69dlc4wfc8i7m8fl6jgp631bn2rla";

  meta = with stdenv.lib; {
    description = "An experimental Rust compiler front-end for IDEs";
    homepage = "https://github.com/rust-analyzer/rust-analyzer";
    license = [ licenses.mit licenses.asl20 ];
    maintainers = [ ];
  };
}

{ stdenv, mozillaRustPlatform, fetchFromGitHub, makeWrapper }:

mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer";
  version = "2020-02-10";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "1iz40iq7xga3rg3ps8m6g5jc899q822j7j43041x89kd96gqg6cl";
  };

  nativeBuildInputs = [ makeWrapper ];

  # Remove usage of rustup for the `codegen` path.
  patches = [ ./use-rustfmt.patch ];

  # Exclude xtask since it depends on rustup
  cargoBuildFlags = [ "--bin ra_cli" "--bin ra_lsp_server" ];

  preBuild = ''
    cargo xtask codegen
  '';

  postInstall = ''
    wrapProgram $out/bin/ra_cli --set "PATH" "$PATH:${mozillaRustPlatform.rust.cargo}/bin/"
    wrapProgram $out/bin/ra_lsp_server --set "PATH" "$PATH:${mozillaRustPlatform.rust.cargo}/bin/"
  '';

  cargoSha256 = "1n4d2a2q2z87kw2f0wnc2yqd12w19xpcrkldf6yp428s6fqp5nhp";

  meta = with stdenv.lib; {
    description = "An experimental Rust compiler front-end for IDEs";
    homepage = "https://github.com/rust-analyzer/rust-analyzer";
    license = [ licenses.mit licenses.asl20 ];
    maintainers = [ ];
  };
}

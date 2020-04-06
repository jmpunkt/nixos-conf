{ stdenv, mozillaRustPlatform, fetchFromGitHub, makeWrapper }:

mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer";
  version = "2020-04-0";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "0cm12707rq88w9yd4kkh26pnaqfvif6yyshk42pfi9vyv4ljfpcv";
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

  cargoSha256 = "16vllp0vf4jhqxaw0dlf1msmg9ak4dga5dqlv1z6n4kbqh1l6sl0";

  meta = with stdenv.lib; {
    description = "An experimental Rust compiler front-end for IDEs";
    homepage = "https://github.com/rust-analyzer/rust-analyzer";
    license = [ licenses.mit licenses.asl20 ];
    maintainers = [ ];
  };
}

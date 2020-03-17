{ stdenv, mozillaRustPlatform, fetchFromGitHub, makeWrapper }:

mozillaRustPlatform.buildRustPackage rec {
  pname = "rust-analyzer";
  version = "2020-03-16";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "0h1dpf9jcdf15qvqmq10giiqmcwdnhw3r8jr26jyh8sk0331i3am";
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

  cargoSha256 = "1bdjim1vdxvb0wlxx5hnaa9snncbp7mi9b7264ffr1xi7ml3mm4z";

  meta = with stdenv.lib; {
    description = "An experimental Rust compiler front-end for IDEs";
    homepage = "https://github.com/rust-analyzer/rust-analyzer";
    license = [ licenses.mit licenses.asl20 ];
    maintainers = [ ];
  };
}

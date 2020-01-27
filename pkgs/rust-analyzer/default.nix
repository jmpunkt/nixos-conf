{ stdenv, mozillaRustPlatform, fetchFromGitHub }:

mozillaRustPlatform.buildRustPackage rec {
  name = "rust-analyzer-${version}";
  version = "2020-01-20";

  src = fetchFromGitHub {
    owner = "rust-analyzer";
    repo = "rust-analyzer";
    rev = "tags/${version}";
    sha256 = "1dx2xlbmymd2wgizcp3v98nxrfj1fw74jwywq3b2fsw0gf3bx1la";
  };

  # Remove usage of rustup for the `codegen` path.
  patches = [ ./use-rustfmt.patch ];

  # Exclude xtask since it depends on rustup
  cargoBuildFlags = ["--bin ra_cli" "--bin ra_lsp_server"];

  preBuild = ''
    cargo xtask codegen
  '';

  cargoSha256 = "0qsil1y2gv8h75x3mnp6h7b295k3icwjhmhriy3bv49qm11ffz0w";

  meta = with stdenv.lib; {
    description = "An experimental Rust compiler front-end for IDEs";
    homepage = https://github.com/rust-analyzer/rust-analyzer;
    license = [ licenses.mit licenses.asl20 ];
    maintainers = [ ];
  };
}

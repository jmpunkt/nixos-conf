{ stdenv, rustPlatform, fetchFromGitHub, pkg-config, openssl }:

rustPlatform.buildRustPackage rec {
  name = "gluon-lsp-${version}";
  version = "0.13.0";

  src = fetchFromGitHub {
    owner = "gluon-lang";
    repo = "gluon_language-server";
    rev = "316c88de73b1c81442f2bf0a07ba5796dc85e11c";
    sha256 = "08qz5d8ydzii995hqb36ra8f9sfcspyz0qjz9fj2zn1ks963ky77";
  };

  cargoSha256 = "0dqhr1mmq5lr3qf5azlgdhw4hib1sb3vi3iflf4l2g2gwyvjak69";

  buildInputs = [ openssl pkg-config ];

  meta = with stdenv.lib; {
    description = "Language server providing completion for gluon ";
    homepage = https://github.com/gluon-lang/gluon_language-server;
    license = licenses.mit;
    maintainers = [ ];
    platforms = platforms.all;
  };
}


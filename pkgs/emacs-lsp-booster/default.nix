{
  fetchFromGitHub,
  rustPlatform,
}:
rustPlatform.buildRustPackage rec {
  pname = "emacs-lsp-booster";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "blahgeek";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-DmEnuAR/OtTdKApEWCdOPAJplT29kuM6ZSHeOnQVo/c=";
  };

  doCheck = false;

  cargoSha256 = "sha256-2wXsPkBl4InjbdYUiiQ+5fZFanLA88t5ApGZ4psfDqk=";
}

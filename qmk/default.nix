{
  stdenv,
  fetchFromGitHub,
  python3,
  qmk,
}: let
  src = fetchFromGitHub {
    repo = "qmk_firmware";
    owner = "qmk";
    rev = "0.23.9";
    fetchSubmodules = true;
    sha256 = "sha256-I1Hw+Lw7fRByDiiA+SWXGZHiuNBe9ohe2Y8EfkxsYus=";
  };
in
  stdenv.mkDerivation rec {
    inherit src;

    name = "keyboard-jmpunkt";

    nativeBuildInputs = [
      python3
      qmk
    ];

    postPatch = ''
      cp -R ${./..}/qmk ./keyboards/sweeeeep
      patchShebangs ./util/uf2conv.py
    '';

    buildPhase = ''
      make sweeeeep:default
    '';

    installPhase = ''
      mkdir -p $out/
      mv sweeeeep_default.uf2  $out/image.uf2
    '';
  }

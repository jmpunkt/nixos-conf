{ lib
, stdenv
, fetchFromGitHub
, cmake
, pkgconfig
, zlib
, SDL2
, SDL2_mixer
, SDL2_image
, SDL2_ttf
, ffmpeg
, boost
, qt5
, minizip
, python37
}:
stdenv.mkDerivation
  rec {
    pname = "vcmi";
    version = "2020-07-05";
    src =
      fetchFromGitHub
        {
          fetchSubmodules = true;
          owner = "vcmi";
          repo = "vcmi";
          rev = "8c77f0488c21cbd6de7c77b34c58654d1d039f21";
          sha256 = "1fkxchyk2l09rn9f83n0mh1s5fw1q3v72x8pjbdaqbqx6rxi48y6";
        };
    nativeBuildInputs = [ cmake pkgconfig python37 ];
    NIX_CFLAGS_COMPILE = "-I${ SDL2_ttf }/include/SDL2 -I${ SDL2_image }/include/SDL2 -I${ SDL2_mixer }/include/SDL2";
    buildInputs = [ qt5.qtbase zlib SDL2.dev SDL2_mixer SDL2_image SDL2_ttf ffmpeg boost minizip ];
    postFixup =
      ''
      patchelf --set-rpath "$(patchelf --print-rpath $out/bin/vcmiclient):$out/lib/vcmi" $out/bin/vcmiclient
      patchelf --set-rpath "$(patchelf --print-rpath $out/bin/vcmilauncher):$out/lib/vcmi" $out/bin/vcmilauncher
      patchelf --set-rpath "$(patchelf --print-rpath $out/bin/vcmiserver):$out/lib/vcmi" $out/bin/vcmiserver
      '';
    meta =
      with lib;
      {
        description = "Open-source engine for Heroes of Might and Magic III";
        homepage = "https://github.com/vcmi/vcmi/";
        license = licenses.gpl2;
        platforms = with platforms; unix;
        maintainers = with maintainers; [ ];
      };
  }

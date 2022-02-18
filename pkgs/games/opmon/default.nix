{ lib
, stdenv
, fetchFromGitHub
, cmake
, pkgconfig
, smpeg
, sfml
}:
stdenv.mkDerivation
  rec {
    pname = "opmon";
    version = "alpha-v0.16";
    src =
      fetchFromGitHub
        {
          owner = "OpMonTeam";
          repo = "OpMon";
          rev = "tags/alpha-v0.16";
          sha256 = "00s0iw4j4w99mn7yq05gqgawgmidrbwh71mcpygsk4k8mpfavz1b";
          fetchSubmodules = true;
        };
    nativeBuildInputs = [ cmake pkgconfig ];
    preConfigure =
      ''
      find ./src -name "*.cpp" -exec sed -i "s!/usr/local/share!$out/share!g" {} +
      '';
    buildInputs = [ smpeg sfml ];
    broken = true;
    meta =
      with lib;
      {
        description = "The free and open source Pokémon clone";
        homepage = "https://github.com/OpMonTeam/OpMon";
        license = licenses.lgpl3;
        platforms = with platforms; unix;
        maintainers = with maintainers; [ ];
      };
  }

{
  lib, stdenv, fetchFromGitHub, cmake, pkgconfig,
  smpeg, sfml
}:

stdenv.mkDerivation rec {
  pname = "opmon";
  version = "alpha-v0.16";

  src = fetchFromGitHub {
    owner = "OpMonTeam";
    repo = "OpMon";
    rev = "tags/alpha-v0.16";
    sha256 = "1vhvh50ca8vmfj5m10q0jbfahm3994l366ca3pgjpzizhqvpcsgz";
  };

  nativeBuildInputs = [
    cmake
    pkgconfig
  ];

  preConfigure = ''
    find ./src -name "*.cpp" -exec sed -i "s!/usr/local/share!$out/share!g" {} +
  '';

  buildInputs = [
    smpeg
    sfml
  ];

  meta = with lib; {
    description = "The free and open source Pokémon clone";
    homepage = https://github.com/OpMonTeam/OpMon;
    license = licenses.lgpl3;
    platforms = with platforms; unix;
    maintainers = with maintainers; [ ];
  };
}

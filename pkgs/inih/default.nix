{ stdenv, fetchFromGitHub, lib, meson, ninja, pkgconfig, cmake }:

stdenv.mkDerivation rec {
  name = "inih";
  version = "51";

  src = fetchFromGitHub {
    owner = "benhoyt";
    repo = "inih";
    rev = "r${version}";
    sha256 = "05mwv6765j2h3z387xryjkz396l0xjhnn4x1d5mhj5lnfrbazz7k";
  };

  nativeBuildInputs = [ ninja meson ];

  mesonFlags = [
    "-Ddefault_library=shared"
    "-Ddistro_install=true"
  ];

  meta = with lib; {
    description =
      "inih (INI Not Invented Here) is a simple .INI file parser written in C";
    homepage = "https://github.com/benhoyt/inih";
    license = with licenses; [ bsd3 ];
    maintainers = with maintainers; [ ];
    platforms = lib.platforms.linux;
  };
}

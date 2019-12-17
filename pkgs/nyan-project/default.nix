{
  lib, stdenv, fetchFromGitHub, cmake, pkgconfig, flex
}:

stdenv.mkDerivation {
  pname = "nyan-project";
  version = "2019-10-18";

  src = fetchFromGitHub {
    owner = "SFTtech";
    repo = "nyan";
    rev = "03a996994b87803b5a8ff6ed0e85ab730163aa2d";
    sha256 = "1zysapcyplfvz3ll91rd981w6zj5ja6c1p00hwig4fzn4xsghyir";
  };

  nativeBuildInputs = [
    cmake
    pkgconfig
  ];

  buildInputs = [
    flex
  ];

  meta = with lib; {
    description = "Modding API with a typesafe hierarchical key-value database with inheritance and dynamic patching";
    homepage = https://github.com/SFTtech/nyan/;
    license = licenses.lgpl3Plus;
    platforms = with platforms; unix;
    maintainers = with maintainers; [ ];
  };
}

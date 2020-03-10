{ lib, stdenv, python37, fetchFromGitHub, cmake, pkgconfig, doxygen
, python37Packages, freetype, SDL2, SDL2_image, opusfile, epoxy, harfbuzz, eigen
, qt5, vulkan-headers, vulkan-loader, libogg, nyan-project }:

stdenv.mkDerivation rec {
  pname = "openage";
  version = "2019-11-30";

  src = fetchFromGitHub {
    owner = "SFTtech";
    repo = "openage";
    rev = "eb8f3c66099f0b07422b5b66a18cdc58adfb9b9f";
    sha256 = "0d67bihm577v52qbp4wa0li9ymlsp13gn02n5akm56dk19lzwx70";
  };

  nativeBuildInputs = [
    cmake
    pkgconfig
    doxygen
    vulkan-headers
    python37Packages.cython
    python37.pkgs.wrapPython
  ];

  buildInputs = [
    python37Packages.pillow
    python37Packages.pygments
    python37Packages.jinja2
    python37Packages.numpy
    freetype
    SDL2
    SDL2_image
    epoxy
    harfbuzz
    eigen
    opusfile
    libogg
    vulkan-loader

    qt5.qtbase
    qt5.qtquickcontrols

    nyan-project
  ];

  outputs = [ "out" "lib" ];

  cmakeFlags = [
    "-DCMAKE_PY_INSTALL_PREFIX=${placeholder "out"}/${python37.sitePackages}"
    "-DGLOBAL_CONFIG_DIR=${placeholder "out"}/etc/openage"
  ];

  postInstall = ''
    wrapProgram "$out/bin/openage" --set PYTHONPATH "$PYTHONPATH:$out/${python37.sitePackages}"
  '';

  meta = with lib; {
    description =
      "Free (as in freedom) open source clone of the Age of Empires II engine";
    homepage = "https://github.com/SFTtech/openage/";
    license = licenses.lgpl3Plus;
    platforms = with platforms; unix;
    maintainers = with maintainers; [ ];
  };
}

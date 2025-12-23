{
  stdenv,
  lib,
  fetchFromGitHub,
  kernel,
  kmod,
}:
stdenv.mkDerivation rec {
  pname = "ec_su_axb35";
  version = "nightly";

  src = fetchFromGitHub {
    owner = "cmetz";
    repo = "ec-su_axb35-linux";
    rev = "8325346a62b9442c1db61959ae1971580b42f42a";
    hash = "sha256-GK0Ykcu80tYYKXTcFo0z8TVKcNcC6Gxp7dEjAHxPIaY=";
  };

  sourceRoot = "source";
  nativeBuildInputs = kernel.moduleBuildDependencies;

  postPatch = ''
    sed -i 's|^KERNEL_BUILD ?= /lib/modules/$(shell uname -r)/build|KERNEL_BUILD := ${kernel.dev}/lib/modules/${kernel.modDirVersion}/build|' Makefile

    # taken from https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=ec-su_axb35-dkms-git
    sed -i '/^obj-m/ s/$/ hwmon\/ec_su_axb35_hwmon.o/' Kbuild
    mv "hwmon/ec-su_axb35-hwmon.c" "hwmon/ec_su_axb35_hwmon.c"
  '';

  makeFlags = [
    "INSTALL_MOD_PATH=$(out)"
  ];

  installPhase = ''
    make modules_install $makeFlags

    mkdir $out/bin
    cp scripts/info.sh $out/bin/su_axb35_info
    cp scripts/test_fan_mode_fixed.sh $out/bin/su_axb35_test
    cp scripts/su_axb35_monitor $out/bin/su_axb35_monitor
  '';

  meta = {
    description = "Linux driver for the embedded controller on the Sixunited AXB35-02 board.";
    homepage = "https://github.com/cmetz/ec-su_axb35-linux";
    license = lib.licenses.gpl2;
    platforms = lib.platforms.linux;
  };
}

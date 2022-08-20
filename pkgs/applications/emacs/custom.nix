{
  pkgs,
  lib,
}:
(pkgs.emacsGit.override {
  withXinput2 = true;
  withPgtk = true;
  nativeComp = true;
  withWebP = true;
  withGTK3 = true;
})
.overrideAttrs (old: rec {
  patches = [
    # keep an eye on (https://github.com/tyler-dodge/emacs/commit/e56c55a742c0f0d152afb4958b863cdf7207b4c3), not working for Linux since it
    # is a MacOS workaround, patch below works fine and has
    # taken from https://github.com/geza-herman/emacs/commit/784a9fd3d511b7f6794f713a8d0b1370ab1b2401
    ./improved-reading.patch
  ];
  configureFlags =
    old.configureFlags
    ++ lib.singleton "--enable-link-time-optimization";
})

{ lib
, mkDerivation
, fetchFromGitLab
, cmake
, extra-cmake-modules
, kconfig
, kio
, mauikit
, mauikit-filebrowsing
, mpv
, knotifications
, qtbase
, qtmultimedia
, qtlocation
, libkexiv2
, kquickimageedit
, wrapQtAppsHook
, qtquickcontrols2
, syntax-highlighting
, taglib
, gst_all_1
}:
let
  mauikit =
    mkDerivation
      rec {
        pname = "mauikit";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "mauikit";
              rev = "v${ version }";
              sha256 = "zTZgRdgehQsN5azgtDCBMp4pjCvOJ7MGAvzD+zArJXc=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules ];
        buildInputs = [ kconfig kio qtquickcontrols2 ];
      };
  mauikit-imagetools =
    mkDerivation
      rec {
        pname = "mauikit-imagetools";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "mauikit-imagetools";
              rev = "v${ version }";
              sha256 = "GRo2U/tHhA3X4EqUwIgjQ0mH3TNQRMy0QozXynXG3H4=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules ];
        buildInputs = [ kconfig kio mauikit qtlocation libkexiv2 kquickimageedit ];
      };
  mauikit-filebrowsing =
    mkDerivation
      rec {
        pname = "mauikit-filebrowsing";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "mauikit-filebrowsing";
              rev = "v${ version }";
              sha256 = "1kuvAgVSTzHX7JzR24S8jlE/z5Eo53B4VPz7AU+5Ggw=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules ];
        buildInputs = [ kconfig kio mauikit ];
      };
  mauikit-texteditor =
    mkDerivation
      rec {
        pname = "mauikit-texteditor";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "mauikit-texteditor";
              rev = "v${ version }";
              sha256 = "UvKXxruCxrroLOsJQuRIWBJu7i6fw7tpHAer3sRxpH0=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules ];
        buildInputs = [ kconfig kio mauikit syntax-highlighting ];
      };
  mauikit-accounts =
    mkDerivation
      rec {
        pname = "mauikit-accounts";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "mauikit-accounts";
              rev = "v${ version }";
              sha256 = "8Tpft4xrqFQxfImibP52GeADEXsIljoK2hojNNwfR9c=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules ];
        buildInputs = [ kconfig kio mauikit ];
      };
  clip =
    mkDerivation
      rec {
        pname = "mauikit-clip";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "clip";
              rev = "v${ version }";
              sha256 = "+n2C289CJ9oDYIwM9BngfBgvgoqKlx/WcREg0ACV+0g=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules wrapQtAppsHook ];
        buildInputs = [ kconfig kio mauikit mauikit-filebrowsing qtmultimedia mpv ];
      };
  pix =
    mkDerivation
      rec {
        pname = "mauikit-pix";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "pix";
              rev = "v${ version }";
              sha256 = "637/CR1NiE/WqqQ9OnC/HPM1qMFDJ5clFPZKQ5tcrIw=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules wrapQtAppsHook ];
        buildInputs = [ kconfig kio mauikit mauikit-filebrowsing mauikit-imagetools qtlocation kquickimageedit libkexiv2 ];
      };
  nota =
    mkDerivation
      rec {
        pname = "mauikit-nota";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "nota";
              rev = "v${ version }";
              sha256 = "On0NkPBevBl41aVKqluv3+O8KsrkBUfPONXn6nE0SCs=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules wrapQtAppsHook ];
        buildInputs = [ kconfig kio mauikit mauikit-filebrowsing mauikit-texteditor ];
      };
  index-fm =
    mkDerivation
      rec {
        pname = "mauikit-index";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "index-fm";
              rev = "v${ version }";
              sha256 = "aY8JBCIh6VyCDOGQIMWhO6asGMo6I6ZTgzpDnnDy9eo=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules wrapQtAppsHook ];
        buildInputs = [ kconfig kio mauikit mauikit-filebrowsing qtquickcontrols2 ];
      };
  vvave =
    mkDerivation
      rec {
        pname = "mauikit-vvave";
        version = "2.0.0";
        src =
          fetchFromGitLab
            {
              domain = "invent.kde.org";
              owner = "maui";
              repo = "vvave";
              rev = "v${ version }";
              sha256 = "Oq3vMcYxCyyTuvk5n72jARPd5alaEslw354D4SjIT3g=";
            };
        nativeBuildInputs = [ cmake extra-cmake-modules wrapQtAppsHook ];
        qtWrapperArgs = [ ''--prefix GST_PLUGIN_PATH : "${
          lib.concatMapStringsSep ":" ( p: "${ p }/lib/gstreamer-1.0" ) gstBuildInputs
        }"'' ];
        gstBuildInputs =
          with gst_all_1;
          [ gstreamer gst-libav gst-vaapi gst-plugins-base gst-plugins-good gst-plugins-bad gst-plugins-ugly ];
        buildInputs =
          [
            kconfig
            kio
            knotifications
            mauikit
            mauikit-filebrowsing
            mauikit-accounts
            qtquickcontrols2
            qtmultimedia
            taglib
          ]
            ++ gstBuildInputs;
      };
in
{ inherit pix clip nota index-fm vvave; }

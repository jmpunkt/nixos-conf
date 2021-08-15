{ stdenv
, fetchFromGitHub
, rustPlatform
, cask
, emacs
, llvmPackages
, clang
, pkgconfig
}:

let
  version = "0.15.1";

  src = fetchFromGitHub {
    owner = "emacs-tree-sitter";
    repo = "elisp-tree-sitter";
    rev = version;
    sha256 = "WgkGtmw63+kRLTRiSEO4bFF2IguH5g4odCujyazkwJc=";
  };

  rust-version =
    rustPlatform.buildRustPackage rec {
      inherit src version;
      name = "tree-sitter-cli-dyn-rust";
      nativeBuildInputs = [ pkgconfig clang ];
      LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";
      cargoSha256 = "JXaAci41qJmS/xzyke3jTUZPSf2phdHEw23Qx4BVZFA=";
    };

  em = (
    emacs.pkgs.withPackages (
      epkgs: (
        with epkgs.melpaStablePackages; [
          # cask
          cask
          s
          f
          commander
          git
          epl
          shut-up
          package-build
          ansi

          # package
          tsc
        ]
      )
    )
  );
in

stdenv.mkDerivation rec {
  inherit version src;
  name = "tree-sitter-cli-dyn";

  nativeBuildInputs = [
    em
  ];

  buildPhase = ''
    caskscript="${em.deps}/bin/cask"

    export HOME=$(mkdir .home)
    export EMACS="${em}/bin/emacs"

    ln -s ${src} .

    mkdir -p "$(bash $caskscript package-directory)"
    # bash $caskscript link tsc core
    # bash $caskscript install

    (
        cd core
        cp -f "${rust-version}/lib/libtsc_dyn.so" "tsc-dyn.so"
        bash $caskscript build
    )
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp core/tsc-dyn.so $out/lib/
    echo "${version}".1 | tr -d $'\n' > $out/lib/DYN-VERSION
  '';
}

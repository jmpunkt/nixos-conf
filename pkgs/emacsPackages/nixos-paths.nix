{ stdenv, lib, buildEnv }:

{ emacs, variables, paths }:
let
  pairs = builtins.concatStringsSep "\n"
    (lib.mapAttrsToList (name: value: ''(setq-default ${name} \"${value}\")'') variables);

  nixos-paths-el = ''
    ;;; nixos-paths.el --- auto generated file by NixOS                -*- lexical-binding: t; -*-

    ;;; Code:
    ${pairs}

    ;;; Add path such that `executable-find` is able to find them
    (add-to-list 'exec-path \"${emacsLibexec}/bin\")
    (setq-default exec-directory \"${emacsLibexec}/bin\")

    ;;; Add path such that `shell-command` can inherit `PATH`
    ;;; NOTICE: append at the end of `PATH` ensuring that users can
    ;;; prepend `PATH` allowing overrides
    (setenv \"PATH\"
            (let ((currentPath (getenv \"PATH\"))
                (additionalPath \"${emacsLibexec}/bin\"))
            (if currentPath
                (concat
                    (getenv \"PATH\")
                    \":\"
                    additionalPath)
                additionalPath)))

    (provide 'nixos-paths)
    ;;; nixos-paths.el ends here
  '';

  emacsPaths = buildEnv {
    inherit paths;
    name = "emacs-paths-deps";
  };

  emacsLibexec = stdenv.mkDerivation {
    name = "emacs-libexec";

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin

      ln -s ${emacsPaths}/bin/* $out/bin
      find ${emacs}/libexec/ -type f -exec ln -s {} $out/bin \;
    '';

    meta = {
      description = "Emulates a `libexec` directory for Emacs which includes the default `libexec` paths and additional dependencies.";
    };
  };
in
stdenv.mkDerivation {
  name = "emacs-nixos-paths";

  phases = [ "buildPhase" "installPhase" ];

  buildPhase = ''
    echo "${nixos-paths-el}" > nixos-paths.el
  '';

  installPhase = ''
    install -d "$out/share/emacs/site-lisp"
    install nixos-paths.el "$out/share/emacs/site-lisp"
  '';

  meta = { description = "NixOS specific paths for Emacs."; };
}

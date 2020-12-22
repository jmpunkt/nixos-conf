{ stdenv, lib, buildEnv, emacsTrivialBuild, writeTextFile, emacs }:

{ variables, paths }:
let
  valueToEmacs = value:
    if (builtins.typeOf value) == "list" then
      "'(${builtins.concatStringsSep "\n" (builtins.map valueToEmacs value)})"
    else if (builtins.isString value) || (builtins.isPath value) then
      ''"${value}"''
    else if (builtins.isNull value) || ((builtins.isBool value) && value == false) then
      "nil"
    else if (builtins.isBool value) then
      "1"
    else if (builtins.isInt value) || (builtins.isFloat value) then
      "${value}"
    else
      lib.assertMsg false "sets are not translated into emacs";

  pairs = builtins.concatStringsSep "\n"
    (lib.mapAttrsToList (name: value: ''(setq-default ${name} ${valueToEmacs value})'') variables);

  nixos-paths-el = ''
    ;;; nixos-paths.el --- auto generated file by NixOS -*- lexical-binding: t; -*-

    ;;; Code:
    ${pairs}

    ;;; Add path such that `executable-find` is able to find them
    (add-to-list 'exec-path "${emacsLibexec}/bin")
    (setq-default exec-directory "${emacsLibexec}/bin")

    ;;; Add path such that `shell-command` can inherit `PATH`
    ;;; NOTICE: append at the end of `PATH` ensuring that users can
    ;;; prepend `PATH` allowing overrides
    (setenv "PATH"
            (let ((currentPath (getenv "PATH"))
                (additionalPath "${emacsLibexec}/bin"))
            (if currentPath
                (concat
                    (getenv "PATH")
                    ":"
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
emacsTrivialBuild {
  pname = "emacs-nixos-paths";
  version = "0.0.1";

  src = writeTextFile {
    name = "nixos-paths.el";
    text = nixos-paths-el;
  };

  meta = { description = "NixOS specific paths for Emacs."; };
}

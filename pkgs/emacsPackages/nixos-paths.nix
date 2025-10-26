{
  buildEnv,
  trivialBuild,
  writeTextFile,
}:
{
  variables,
  paths,
}:
let
  nixos-paths-el = ''
    ;;; nixos-paths.el --- auto generated file by NixOS -*- lexical-binding: t; -*-

    ;;; Code:
    ${variables}

    ;;; Add path such that `executable-find` is able to find them
    (add-to-list 'exec-path "${emacsPaths}/bin")

    ;;; Add path such that `shell-command` can inherit `PATH`
    ;;; NOTICE: append at the end of `PATH` ensuring that users can
    ;;; prepend `PATH` allowing overrides
    (setenv "PATH"
            (let ((currentPath (getenv "PATH"))
                (additionalPath "${emacsPaths}/bin"))
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
    pathsToLink = [ "/bin" ];
  };
in
trivialBuild {
  pname = "emacs-nixos-paths";
  version = "0.0.1";
  src = writeTextFile {
    name = "nixos-paths.el";
    text = nixos-paths-el;
  };
  meta = {
    description = "NixOS specific paths for Emacs.";
  };
}

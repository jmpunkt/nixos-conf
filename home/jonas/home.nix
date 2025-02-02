{
  config,
  systemConfig,
  pkgs,
  lib,
  ...
}: {
  imports =
    (import ../../modules/all-home-manager.nix)
    ++ [
      (
        if systemConfig.programs.hyprland.enable
        then import ./hyprland
        else {...}: {}
      )
    ];
  home.language = {
    base = "en_IE.UTF-8";
    monetary = "de_DE.utf8";
    telephone = "de_DE.utf8";
  };
  xdg.configFile = {
    "emacs/init.el".text = builtins.readFile ./emacs/init.el;
    # since emacs 27.1
    "emacs/early-init.el".text = builtins.readFile ./emacs/early-init.el;
  };
  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
    ".ssh/config".text = builtins.readFile ./ssh/config;
  };
  manual.manpages.enable = false;
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    NIXOS_OZONE_WL = 1;
    GTAGSCONF = "${pkgs.global}/share/gtags/gtags.conf";
    GTAGSLABEL = "pygments";
    MAKEOBJDIRPREFIX = "/home/jonas/.cache/gtags";
  };
  systemd.user.tmpfiles.rules = [
    "D /home/jonas/.cache/gtags 0755 jonas jonas 7d -"
  ];
  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
  programs = {
    fish.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    git = {
      enable = true;
      userName = "Jonas Meurer";
      userEmail = "jmpunkt@outlook.com";
      lfs.enable = true;
      signing = {
        key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
        signByDefault = true;
      };
      extraConfig = {
        core = {
          whitespace = "trailing-space,space-before-tab";
        };
        rerere.enabled = "true";
        merge.conflictstyle = "zdiff3";
        pull.ff = "only";
      };
    };
    emacs = {
      enable = true;
      package = pkgs.jmpunkt.emacs;
      extraPackages = epkgs:
        with epkgs.melpaPackages;
          [
            # Core
            avy
            meow
            which-key
            hl-todo
            envrc
            helpful
            xterm-color
            editorconfig
            reformatter
            gcmh
            biome
            casual-avy
            transient-dwim
            rg
            corfu
            corfu-prescient
            # docker
            citre
            # Org
            ob-async
            ob-mermaid
            ob-graphql
            # PDF
            pdf-tools
            # Git
            magit
            diff-hl
            # UI
            modus-themes
            # Bibliography
            citar
            # Search/Find
            consult
            vertico
            vertico-prescient
            embark
            embark-consult
            marginalia
            cape
            tempel
            # Spelling
            flyspell-correct
            languagetool
            # RSS
            elfeed
            # Misc
            yaml-mode
            graphql-mode
            mermaid-mode
            # Presentation
            ox-typst
            markdown-mode
            graphviz-dot-mode
            # Programming
            rust-mode
            nix-ts-mode
            nix-mode
            devdocs
            flymake-ruff
          ]
          ++ (with epkgs.elpaPackages; [
            undo-tree
            org
            breadcrumb
          ])
          ++ (with epkgs.nongnuPackages; [eat])
          ++ (
            with epkgs.manualPackages;
              [
                jmpunktPkgs.eglot-x
                jmpunktPkgs.typst-ts-mode
                jmpunktPkgs.copilot
                jmpunktPkgs.xdg-appmenu
                jmpunktPkgs.p-search
                jmpunktPkgs.ultra-scroll
                (jmpunktPkgs.nixosPaths (import ./emacs/variables.nix {inherit pkgs;}))
              ]
              ++ [
                treesit-grammars.with-all-grammars
              ]
          );
    };
  };
  services.emacs = {
    # enable = true;
    startWithUserSession = "graphical";
    client.enable = true;
    defaultEditor = true;
  };
  home.stateVersion = "18.09";
}

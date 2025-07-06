{pkgs, ...}: {
  xdg.configFile = {
    "emacs/init.el".text = builtins.readFile ./init.el;
    # since emacs 27.1
    "emacs/early-init.el".text = builtins.readFile ./early-init.el;
  };
  services.emacs = {
    # enable = true;
    startWithUserSession = "graphical";
    client.enable = true;
    defaultEditor = true;
  };
  programs.emacs = {
    enable = true;
    package = pkgs.jmpunkt.emacs;
    extraPackages = epkgs:
      with epkgs.melpaPackages;
        [
          # Core
          avy
          meow
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
          jinx
          languagetool
          # RSS
          elfeed
          # Misc
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
          svg-lib
          kind-icon
        ])
        ++ (with epkgs.nongnuPackages; [eat])
        ++ (
          with epkgs.manualPackages;
            [
              jmpunktPkgs.eglot-x
              jmpunktPkgs.reader
              jmpunktPkgs.typst-ts-mode
              jmpunktPkgs.copilot
              jmpunktPkgs.p-search
              (jmpunktPkgs.nixosPaths (import ./variables.nix {inherit pkgs;}))
            ]
            ++ [
              treesit-grammars.with-all-grammars
            ]
        );
  };
}

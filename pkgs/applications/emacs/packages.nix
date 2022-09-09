{pkgs}: let
in
  epkgs:
    with epkgs.melpaPackages;
      [
        esup
        # Core
        meow
        use-package
        which-key
        hl-todo
        envrc
        dashboard
        smart-jump
        tree-sitter-langs
        helpful
        xterm-color
        # Org
        org-bullets
        ob-async
        ob-mermaid
        # PDF
        pdf-tools
        # Git
        magit
        diff-hl
        # UI
        all-the-icons
        doom-modeline
        doom-themes
        # Bibliography
        citar
        # SQL
        sqlformat
        # Search/Find
        consult
        orderless
        affe
        embark
        embark-consult
        marginalia
        cape
        yasnippet
        # Spelling
        langtool
        flyspell-correct
        # RSS
        elfeed
        # LSP
        eglot
        # Misc
        yaml-mode
        fish-mode
        nix-mode
        graphql-mode
        mermaid-mode
        # Presentation
        markdown-mode
        graphviz-dot-mode
        # Programming
        rust-mode
        haskell-mode
        prettier-js
      ]
      ++ (with epkgs.elpaPackages; [undo-tree seq vertico org])
      ++ (
        with epkgs.manualPackages; [
          tree-sitter-indexer-elisp # custom package
          jmpunktPkgs.ligature
          jmpunktPkgs.eglot-x
          (jmpunktPkgs.nixosPaths (import ./variables.nix {inherit pkgs;}))
        ]
      )

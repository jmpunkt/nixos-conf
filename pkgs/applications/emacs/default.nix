{ stdenv
, buildEnv
, pkgs
, emacsGcc
, emacsPackagesFor
, symlinkJoin
, plantuml
, nodePackages
, vscode-utils
, nodejs
}:
let
  emacs = emacsGcc;

  vscode-eslint = vscode-utils.buildVscodeMarketplaceExtension {
    mktplcRef = {
      name = "vscode-eslint";
      publisher = "dbaeumer";
      # https://github.com/emacs-lsp/lsp-mode/issues/1932#issuecomment-714633460
      version = "2.1.8";
      sha256 = "parXZhF9qyRAlmeGItCbvPfyyQQ9WmlBKKFYQ8KIFH0=";
    };
    meta = {
      license = stdenv.lib.licenses.mit;
    };
  };

  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
in
(emacsPackagesFor emacs).emacsWithPackages (
  epkgs:
    with epkgs.melpaPackages; [
      # Core
      evil
      use-package
      smartparens
      which-key
      hl-todo
      direnv
      dashboard
      projectile

      # Org
      org-fancy-priorities
      org-bullets
      org-ref
      org-noter
      ob-async
      ob-mermaid
      toc-org
      pdf-tools

      # Bibliography
      biblio
      biblio-core
      parsebib

      # PDF
      pdf-tools

      # Git
      magit
      magit-todos
      gitattributes-mode
      gitconfig-mode
      gitignore-mode
      evil-magit
      diff-hl

      # Tree
      treemacs
      treemacs-projectile
      treemacs-evil

      # Templates
      yasnippet
      yasnippet-snippets

      # UI
      all-the-icons
      doom-modeline
      doom-themes

      # Linter
      flycheck
      flycheck-irony

      # Search/Find
      ivy
      ivy-bibtex
      ivy-prescient
      ivy-xref
      counsel
      counsel-projectile

      # Spelling
      langtool
      flyspell-correct
      flyspell-correct-ivy

      # Completion
      company
      company-bibtex
      company-math
      company-web
      company-quickhelp
      company-reftex
      company-prescient

      # LSP
      lsp-ui
      lsp-java
      lsp-ivy
      lsp-treemacs
      lsp-metals

      # Configuration
      bazel-mode
      yaml-mode
      json-mode
      fish-mode
      nix-mode
      graphql-mode
      meson-mode
      mermaid-mode
      dhall-mode
      sbt-mode

      # Presentation
      markdown-mode
      pandoc-mode
      graphviz-dot-mode

      # Programming
      irony
      rustic
      haskell-mode
      web-mode
      scala-mode
      prettier-js
    ]
    ++ (with epkgs.elpaPackages; [ undo-tree auctex seq ])
    ++ (with epkgs.orgPackages; [ org org-plus-contrib ])
    ++ (
      with epkgs.manualPackages; [
        jmpunkt.ligature
        (
          jmpunkt.nixosPaths
            {
              inherit emacs;
              variables = {
                org-plantuml-jar-path = "${plantuml}/lib/plantuml.jar";
                ob-mermaid-cli-path = "${nodePackages.jmpunkt.mermaid-cli}/bin/mmdc";
                mermaid-mmdc-location = "${nodePackages.jmpunkt.mermaid-cli}/bin/mmdc";
                lsp-eslint-server-command = [
                  "${nodejs}/bin/node"
                  "${pathOfExtension vscode-eslint}/server/out/eslintServer.js"
                  "--stdio"
                ];
                prettier-js-command = "${nodePackages.prettier}/bin/prettier";
              };
              paths = with pkgs; [
                inkscape # org-mode:graphs
                imagemagick # org-mode:graphs
                graphviz-nox # org-mode:graphs
                nodePackages.jmpunkt.mermaid-cli # org-mode:graphs
                jre # required by plantuml
                ghostscript # LaTex EPS files
                unoconv # org-mode:odt
                zip # org-mode:odt
                languagetool # spelling
                nixpkgs-fmt # dev
                ccls # LSP
                rnix-lsp # LSP
                haskellPackages.haskell-lsp # LSP
                nodePackages.typescript-language-server # LSP
                texlab # LSP
                yaml-language-server # LSP
                nodePackages.typescript # dev
                discount
                maven # dev
                metals # LSP
                sbt # Scala
                cargo-outdated # dev
                git # treemacs

                jmpunkt.pythonToolchain # overlays/40-toolchains.nix
                jmpunkt.rustToolchain.stable # overlays/40-toolchains.nix
                jmpunkt.latex # custom latex suite
              ];
            }
        )
      ]
    )
)

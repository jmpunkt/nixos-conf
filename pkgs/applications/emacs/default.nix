{ stdenv
, buildEnv
, pkgs
, emacsGcc
, emacsPackagesFor
, symlinkJoin
, plantuml
, nodePackages
, nodejs
, vscode-extensions
, unstable
}:
let
  emacs = emacsGcc;

  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
in
(emacsPackagesFor emacs).emacsWithPackages (
  epkgs:
    with epkgs.melpaPackages; [
      # Core
      esup
      evil
      evil-collection
      use-package
      smartparens
      which-key
      hl-todo
      direnv
      dashboard
      projectile

      # Org
      org-bullets
      org-noter
      ob-async
      ob-mermaid
      toc-org

      # PDF
      pdf-tools

      # Git
      magit
      magit-todos
      diff-hl

      # Tree
      treemacs
      treemacs-projectile
      treemacs-evil
      treemacs-magit

      # Templates
      yasnippet
      yasnippet-snippets

      # UI
      all-the-icons
      doom-modeline
      doom-themes

      # Bibliography
      bibtex-actions

      # Linter
      flycheck

      # Search/Find
      selectrum
      consult
      orderless

      # Spelling
      langtool
      flyspell-correct

      # Completion
      company
      company-math
      company-web

      # RSS
      elfeed

      # LSP
      lsp-ui
      lsp-treemacs
      lsp-metals

      # Misc
      bazel
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
      graphviz-dot-mode

      # Programming
      rustic
      haskell-mode
      web-mode
      scala-mode
      prettier-js
    ]
    ++ (with epkgs.elpaPackages; [ undo-tree seq ])
    ++ (with epkgs.orgPackages; [ org org-plus-contrib ])
    ++ (
      with epkgs.manualPackages; [
        jmpunkt.ligature
        (
          jmpunkt.nixosPaths
            {
              variables = {
                org-plantuml-jar-path = "${plantuml}/lib/plantuml.jar";
                ob-mermaid-cli-path = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
                mermaid-mmdc-location = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
                lsp-eslint-server-command = [
                  "${nodejs}/bin/node"
                  "${pathOfExtension vscode-extensions.jmpunkt.vscode-eslint}/server/out/eslintServer.js"
                  "--stdio"
                ];
                prettier-js-command = "${nodePackages.prettier}/bin/prettier";
              };
              paths = with pkgs; [
                gitAndTools.delta # magit-delta
                inkscape # org-mode:graphs
                imagemagick # org-mode:graphs
                graphviz-nox # org-mode:graphs
                unstable.nodePackages.mermaid-cli # org-mode:graphs
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

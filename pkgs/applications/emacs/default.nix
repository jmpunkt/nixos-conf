{ lib
, stdenv
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
, jmpunkt
}:
let
  emacs = emacsGcc;

  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";

  treeSitterGrammars = pkgs.runCommandLocal "tree-sitter-grammars-bundle" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
    (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${(builtins.replaceStrings [ "tree-sitter-" ] [ "" ] name)}.so") pkgs.tree-sitter.builtGrammars)};
  '';
in
(emacsPackagesFor emacs).emacsWithPackages (
  epkgs:
    with epkgs.melpaPackages; [
      # Core
      evil
      evil-collection
      use-package
      smartparens
      which-key
      hl-todo
      envrc
      dashboard
      projectile

      tree-sitter
      tree-sitter-langs

      # Org
      org-bullets
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

      # UI
      all-the-icons
      doom-modeline
      doom-themes

      # Bibliography
      citar

      # Search/Find
      consult
      orderless
      affe
      embark
      embark-consult
      marginalia

      # Spelling
      langtool
      flyspell-correct

      # RSS
      elfeed

      # LSP
      eglot

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

      # Presentation
      markdown-mode
      graphviz-dot-mode

      # Programming
      rust-mode
      haskell-mode
      typescript-mode
      prettier-js
    ]
    ++ (with epkgs.elpaPackages; [ undo-tree seq vertico corfu ])
    ++ (with epkgs.orgPackages; [ org org-plus-contrib ])
    ++ (
      with epkgs.manualPackages; [
        jmpunktPkgs.ligature
        (
          jmpunktPkgs.nixosPaths
            {
              variables = {
                org-plantuml-jar-path = "${plantuml}/lib/plantuml.jar";
                ob-mermaid-cli-path = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
                mermaid-mmdc-location = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
                lsp-eslint-server-command = [
                  "${nodejs}/bin/node"
                  "${pathOfExtension vscode-extensions.jmpunktPkgs.vscode-eslint}/server/out/eslintServer.js"
                  "--stdio"
                ];
                prettier-js-command = "${nodePackages.prettier}/bin/prettier";
                tree-sitter-load-path = [ "${treeSitterGrammars}/bin" ];
                tsc-dyn-dir = "${jmpunkt.tsc-dyn}/lib";
                # HACK: prevents tree sitter from compiling grammars
                tree-sitter-langs--testing = true;
              };
              paths = with pkgs; [
                ccls # LSP
                rnix-lsp # LSP
                texlab # LSP
                yaml-language-server # LSP
                haskellPackages.haskell-lsp # LSP
                nodePackages.typescript-language-server # LSP

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
                nodePackages.typescript # dev
                discount
                maven # dev
                cargo-outdated # dev
                git # treemacs
                fd # affe
                ripgrep #affe

                jmpunkt.pythonToolchain # overlays/40-toolchains.nix
                jmpunkt.latex # custom latex suite
              ];
            }
        )
      ]
    )
)

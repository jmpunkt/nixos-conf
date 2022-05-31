{
  lib,
  stdenv,
  buildEnv,
  pkgs,
  emacsPackagesFor,
  symlinkJoin,
  plantuml,
  nodePackages,
  nodejs,
  vscode-extensions,
  unstable,
  jmpunkt,
}: let
  emacs =
    (pkgs.emacsGit.override {
      withXinput2 = true;
      withPgtk = true;
      nativeComp = true;
      withToolkitScrollBars = false;
      withWebP = true;
      withGTK3 = true;
    })
    .overrideAttrs (old: rec {
      patches = [
        # keep an eye on (https://github.com/tyler-dodge/emacs/commit/e56c55a742c0f0d152afb4958b863cdf7207b4c3), not working for Linux since it
        # is a MacOS workaround, patch below works fine and has
        # taken from https://github.com/geza-herman/emacs/commit/784a9fd3d511b7f6794f713a8d0b1370ab1b2401
        ./improved-reading.patch
      ];
      configureFlags =
        old.configureFlags
        ++ lib.singleton "--enable-link-time-optimization";
    });
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
  tomlHighlights =
    pkgs.writeText "toml-highlights.scm"
    ''
      (bare_key) @property
      (quoted_key) @string

      (boolean) @keyword
      (comment) @comment
      (string) @string
      (integer) @number
      (float) @number
      (offset_date_time) @string.special
      (local_date_time) @string.special
      (local_date) @string.special
      (local_time) @string.special

      "." @punctuation.delimiter
      "," @punctuation.delimiter

      "=" @operator

      "[" @punctuation.bracket
      "]" @punctuation.bracket
      "[[" @punctuation.bracket
      "]]" @punctuation.bracket
      "{" @punctuation.bracket
      "}" @punctuation.bracket
    '';
  treeSitterGrammars =
    pkgs.runCommandLocal
    "tree-sitter-grammars-bundle"
    {}
    ''
      mkdir -p $out/bin
      ${
        lib.concatStringsSep
        "\n"
        (
          lib.mapAttrsToList
          (
            name: src: "ln -s ${src}/parser $out/bin/${(builtins.replaceStrings ["tree-sitter-"] [""] name)}.so"
          )
          pkgs.tree-sitter.builtGrammars
        )
      };
    '';
in
  (emacsPackagesFor emacs).emacsWithPackages
  (
    epkgs:
      with epkgs.melpaPackages;
        [
          # Core
          evil
          evil-collection
          evil-textobj-tree-sitter
          use-package
          which-key
          hl-todo
          envrc
          dashboard
          smart-jump
          tree-sitter
          (tree-sitter-langs.overrideAttrs (old: rec {
            postInstall =
              (old.postInstall or "")
              + ''
                mkdir $out/share/emacs/site-lisp/elpa/${old.pname}-${old.version}/queries/toml
                ln -s ${tomlHighlights} $out/share/emacs/site-lisp/elpa/${old.pname}-${old.version}/queries/toml/highlights.scm
              '';
          }))
          vterm
          xterm-color
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
        ++ (with epkgs.elpaPackages; [undo-tree seq vertico corfu org])
        ++ (
          with epkgs.manualPackages; [
            jmpunktPkgs.ligature
            jmpunktPkgs.eglot-x
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
                  tree-sitter-load-path = ["${treeSitterGrammars}/bin"];
                  # HACK: prevents tree sitter from compiling grammars
                  tree-sitter-langs--testing = true;
                  nix-nixfmt-bin = "${unstable.alejandra}/bin/alejandra";
                };
                paths = with pkgs; [
                  ccls
                  # LSP
                  rnix-lsp
                  # LSP
                  texlab
                  # LSP
                  yaml-language-server
                  # LSP
                  # haskellPackages.haskell-lsp
                  # LSP
                  nodePackages.typescript-language-server
                  # LSP
                  gitAndTools.delta
                  # magit-delta
                  inkscape
                  # org-mode:graphs
                  imagemagick
                  # org-mode:graphs
                  graphviz-nox
                  # org-mode:graphs
                  unstable.nodePackages.mermaid-cli
                  # org-mode:graphs
                  jre
                  # required by plantuml
                  ghostscript
                  # LaTex EPS files
                  unoconv
                  # org-mode:odt
                  zip
                  # org-mode:odt
                  languagetool
                  # spelling
                  nixpkgs-fmt
                  # dev
                  nodePackages.typescript
                  # dev
                  discount
                  maven
                  # dev
                  cargo-outdated
                  # dev
                  git
                  fd
                  # affe
                  ripgrep
                  #affe
                  jmpunkt.pythonToolchain
                  # overlays/40-toolchains.nix
                  jmpunkt.latex
                  # custom latex suite
                  # SQL
                  pgformatter
                ];
              }
            )
          ]
        )
  )

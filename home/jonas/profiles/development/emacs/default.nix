{
  systemConfig,
  pkgs,
  lib,
  ...
}:
let
  cfg = systemConfig.profiles.development;
in
{
  config = lib.mkIf cfg.enable {
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
      extraPackages =
        epkgs:
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
          biome
          casual
          casual-avy
          transient-dwim
          rg
          titlecase
          caser
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
          # Programming
          nix-ts-mode
          nix-mode
          devdocs
          flymake-ruff
          d2-mode
          (typespec-ts-mode.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = "jmpunkt";
              repo = "typespec-ts-mode";
              rev = "91fb990093ea8db8e1f30114ea52a8d6afbda5f9";
              sha256 = "sha256-cUy66yiwxU+X1cBK1THJS9oQsDL4IlN9MEK3TU3XGvc=";
            };
          }))
        ]
        ++ (with epkgs.elpaPackages; [
          so-long
          undo-tree
          org
          breadcrumb
          svg-lib
          kind-icon
        ])
        ++ (with epkgs.nongnuPackages; [ eat ])
        ++ (
          with epkgs.manualPackages;
          [
            jmpunktPkgs.eglot-x
            jmpunktPkgs.reader
            jmpunktPkgs.typst-ts-mode
            jmpunktPkgs.copilot
            jmpunktPkgs.nix-update-el
          ]
          ++ [
            (treesit-grammars.with-grammars (
              p: (builtins.attrValues p) ++ [ pkgs.jmpunkt.tree-sitter-typespec ]
            ))
          ]
        );

      variables =
        let
          copilot = pkgs.nodePackages.jmpunkt."@github/copilot-language-server-1.364.0";
          copilot-run = pkgs.writeShellScriptBin "copilot-run" ''
            "${pkgs.nodejs}/bin/node" "${copilot}/lib/node_modules/@github/copilot-language-server/dist/language-server.js" "$@"
          '';
        in
        {
          org-plantuml-jar-path = "${pkgs.plantuml}/lib/plantuml.jar";
          ob-mermaid-cli-path = "${pkgs.nodePackages.mermaid-cli}/bin/mmdc";
          mermaid-mmdc-location = "${pkgs.nodePackages.mermaid-cli}/bin/mmdc";
          languagetool-server-command = "${pkgs.languagetool}/share/languagetool-server.jar";
          languagetool-console-command = "${pkgs.languagetool}/share/languagetool-commandline.jar";
          languagetool-java-bin = "${pkgs.jre}/bin/java";
          copilot-install-dir = "${copilot}";
          copilot-server-executable = "${copilot-run}/bin/copilot-run";
          copilot-version = copilot.version;
          svg-lib-icon-collections = {
            "\"material\"" = "file://${
              pkgs.fetchFromGitHub {
                owner = "Templarian";
                repo = "MaterialDesign";
                rev = "2424e748e0cc63ab7b9c095a099b9fe239b737c0";
                sha256 = "sha256-QMGl7soAhErrrnY3aKOZpt49yebkSNzy10p/v5OaqQ0=";
              }
            }/svg/%s.svg";
          };
        };
      paths =
        let
          core = with pkgs; [
            fd # find file
            ripgrep # search
            pandoc # markdown, etc
            git
            git-absorb
            universal-ctags
            global
            d2 # d2lang
          ];
          lsp = with pkgs; [
            ccls
            yaml-language-server
            nodePackages.typescript-language-server
            unstable.nixd
            unstable.tinymist # Typst
            unstable.basedpyright
            taplo # Toml
            unstable.typespec
          ];
          org = with pkgs; [
            # graphs
            inkscape
            imagemagick
            graphviz-nox
            pkgs.nodePackages.mermaid-cli
            ghostscript
            zip
            tectonic
            typst
          ];
          formatter = with pkgs; [
            nixpkgs-fmt
            pgformatter
            nodePackages.prettier
            nixfmt-rfc-style
            typstyle
            ruff
            biome
            (rustfmt.override { asNightly = true; })
          ];
        in
        core ++ org ++ lsp ++ formatter;

    };
  };
}

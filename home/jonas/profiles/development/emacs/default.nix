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
      # NOTE: Prefer the builtin version for these packages. Some builtin
      # packages might be newer than their external counter part.
      overrides = self: super: {
        jsonrpc = null;
        eglot = null;
        org = null;
        xref = null;
        seq = null;
        editorconfig = null;
        let-alist = null;
        transient = null;
      };
      extraPackages =
        epkgs:
        with epkgs.melpaPackages;
        [
          # AI
          gptel
          agent-shell
          # Core
          avy
          meow
          hl-todo
          envrc
          helpful
          xterm-color
          reformatter
          casual
          casual-avy
          transient-dwim
          rg
          titlecase
          caser
          citre
          # Org
          ob-async
          ob-mermaid
          ob-graphql
          # PDF
          pdf-tools
          # Git
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
          typespec-ts-mode
          jira
        ]
        ++ (with epkgs.elpaPackages; [
          so-long
          undo-tree
          breadcrumb
          svg-lib
          kind-icon
          transient
        ])
        ++ (with epkgs.nongnuPackages; [
          eat
          magit
        ])
        ++ (
          with epkgs.manualPackages;
          [
            jmpunktPkgs.eglot-x
            jmpunktPkgs.reader
            jmpunktPkgs.typst-ts-mode
            jmpunktPkgs.copilot
            jmpunktPkgs.nix-update-el
            jmpunktPkgs.eglot-booster
          ]
          ++ [
            (treesit-grammars.with-grammars (
              p: (builtins.attrValues p) ++ [ pkgs.jmpunkt.tree-sitter-typespec ]
            ))
          ]
        );

      variables = {
        org-plantuml-jar-path = "${pkgs.plantuml}/lib/plantuml.jar";
        ob-mermaid-cli-path = "${pkgs.mermaid-cli}/bin/mmdc";
        mermaid-mmdc-location = "${pkgs.mermaid-cli}/bin/mmdc";
        languagetool-server-command = "${pkgs.languagetool}/share/languagetool-server.jar";
        languagetool-console-command = "${pkgs.languagetool}/share/languagetool-commandline.jar";
        languagetool-java-bin = lib.getExe pkgs.jre;
        copilot-install-dir =
          let
            pname = "@github/copilot-language-server";
            version = pkgs.copilot-language-server.version;
          in
          pkgs.runCommand "copilot-language-server-${version}" { } ''
            mkdir -p $out/lib/node_modules/${pname}
            mkdir -p $out/lib64/node_modules/${pname}

            cat > $out/lib/node_modules/${pname}/package.json <<EOF
            {
              "name": "${pname}",
              "version": "${version}"
            }
            EOF

            cp $out/lib/node_modules/${pname}/package.json \
               $out/lib64/node_modules/${pname}/package.json
          '';
        copilot-server-executable = lib.getExe pkgs.copilot-language-server;
        copilot-lsp-server-version = pkgs.copilot-language-server.version;
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
            typescript-language-server
            unstable.nixd
            unstable.tinymist # Typst
            unstable.basedpyright
            taplo # Toml
            unstable.typespec
            emacs-lsp-booster
          ];
          org = with pkgs; [
            # graphs
            inkscape
            imagemagick
            graphviz-nox
            mermaid-cli
            ghostscript
            zip
            tectonic
            typst
          ];
          formatter = with pkgs; [
            nixpkgs-fmt
            pgformatter
            prettier
            nixfmt
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

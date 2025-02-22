{pkgs}: let
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
  vscode-eslint = pkgs.vscode-extensions.jmpunktPkgs.vscode-eslint;
  eslint-lsp = pkgs.writeShellScriptBin "eslint-lsp" ''
    "${pkgs.nodejs}/bin/node" "${pathOfExtension vscode-eslint}/server/out/eslintServer.js" "--stdio"
  '';
  copilot =
    pkgs.nodePackages.jmpunkt."@github/copilot-language-server-1.273.0";
  copilot-run = pkgs.writeShellScriptBin "copilot-run" ''
    "${pkgs.nodejs}/bin/node" "${copilot}/lib/node_modules/@github/copilot-language-server/dist/language-server.js" "$@"
  '';
in {
  variables = {
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
      "\"material\"" = "file://${pkgs.fetchFromGitHub {
        owner = "Templarian";
        repo = "MaterialDesign";
        rev = "2424e748e0cc63ab7b9c095a099b9fe239b737c0";
        sha256 = "sha256-QMGl7soAhErrrnY3aKOZpt49yebkSNzy10p/v5OaqQ0=";
      }}/svg/%s.svg";
    };
  };
  paths = let
    core = with pkgs; [
      fd # find file
      ripgrep # search
      pandoc # markdown, etc
      git
      git-absorb
      universal-ctags
      global
    ];
    lsp = with pkgs; [
      ccls
      yaml-language-server
      nodePackages.typescript-language-server
      eslint-lsp
      nixd
      typst-lsp
      basedpyright
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
      alejandra
      typstyle
      ruff
      biome
      (rustfmt.override {asNightly = true;})
    ];
  in
    core ++ org ++ lsp ++ formatter;
}

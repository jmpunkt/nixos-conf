{pkgs}: let
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
  vscode-eslint = pkgs.vscode-extensions.jmpunktPkgs.vscode-eslint;
  eslint-lsp = pkgs.writeShellScriptBin "eslint-lsp" ''
    "${pkgs.nodejs}/bin/node" "${pathOfExtension vscode-eslint}/server/out/eslintServer.js" "--stdio"
  '';
  copilot =
    pkgs.nodePackages_latest.jmpunkt."copilot-node-server-1.14.0";
in {
  variables = {
    org-plantuml-jar-path = "${pkgs.plantuml}/lib/plantuml.jar";
    ob-mermaid-cli-path = "${pkgs.nodePackages.mermaid-cli}/bin/mmdc";
    mermaid-mmdc-location = "${pkgs.nodePackages.mermaid-cli}/bin/mmdc";
    languagetool-server-command = "${pkgs.languagetool}/share/languagetool-server.jar";
    languagetool-console-command = "${pkgs.languagetool}/share/languagetool-commandline.jar";
    languagetool-java-bin = "${pkgs.jre}/bin/java";
    copilot-node-executable = "${pkgs.nodePackages_latest.nodejs}/bin/node";
    copilot-install-dir = "${copilot}";
    copilot-version = copilot.version;
  };
  paths = let
    core = with pkgs; [
      fd # find file
      ripgrep # search
      pandoc # markdown, etc
      git
      typst
      git-absorb
      universal-ctags
      global
    ];
    lsp = with pkgs; [
      ccls
      yaml-language-server
      nodePackages.typescript-language-server
      eslint-lsp
      nil
      typst-lsp
      # NOTE: not a single application
      (python3.withPackages (p: (with p; [
        python-lsp-server
        python-lsp-ruff
      ])))
    ];
    org = with pkgs; [
      # graphs
      inkscape
      imagemagick
      graphviz-nox
      pkgs.nodePackages.mermaid-cli
      ghostscript
      zip
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

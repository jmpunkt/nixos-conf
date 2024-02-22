{pkgs}: let
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
  vscode-eslint = pkgs.vscode-extensions.jmpunktPkgs.vscode-eslint;
  eslint-lsp = pkgs.writeShellScriptBin "eslint-lsp" ''
    "${pkgs.nodejs}/bin/node" "${pathOfExtension vscode-eslint}/server/out/eslintServer.js" "--stdio"
  '';
in {
  variables = {
    org-plantuml-jar-path = "${pkgs.plantuml}/lib/plantuml.jar";
    ob-mermaid-cli-path = "${pkgs.nodePackages.mermaid-cli}/bin/mmdc";
    mermaid-mmdc-location = "${pkgs.nodePackages.mermaid-cli}/bin/mmdc";
    flymake-languagetool-server-command = ["${pkgs.languagetool}/bin/languagetool-http-server"];
    parinfer-rust-library = "${pkgs.kakounePlugins.parinfer-rust}/lib/libparinfer_rust.so";
  };
  paths = let
    core = with pkgs; [
      fd # find file
      ripgrep # search
      pandoc # markdown, etc
      git
      typst
      jmpunkt.emacs-lsp-booster # eglot, lsp
    ];
    lsp = with pkgs; [
      ccls
      rnix-lsp
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
      typstfmt
      ruff
      biome
      (rustfmt.override {asNightly = true;})
    ];
  in
    core ++ org ++ lsp ++ formatter;
}

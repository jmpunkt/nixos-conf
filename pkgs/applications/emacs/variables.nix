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
  };
  paths = let
    core = with pkgs; [
      fd # find file
      ripgrep # search
      pandoc # markdown, etc
      git
    ];
    lsp = with pkgs; [
      ccls
      rnix-lsp
      texlab
      yaml-language-server
      # haskellPackages.haskell-lsp
      nodePackages.typescript-language-server
      jmpunkt.pythonToolchain
      jmpunkt.latex
      eslint-lsp
      nil
      typst
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
      pkgs.alejandra
    ];
  in
    core ++ org ++ lsp ++ formatter;
}

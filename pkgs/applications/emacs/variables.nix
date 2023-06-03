{pkgs}: let
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
  vscode-eslint = pkgs.vscode-extensions.jmpunktPkgs.vscode-eslint;
  eslint-lsp = pkgs.writeShellScriptBin "eslint-lsp" ''
    "${pkgs.nodejs}/bin/node" "${pathOfExtension vscode-eslint}/server/out/eslintServer.js" "--stdio"
  '';
in {
  variables = let
    inherit (pkgs) plantuml unstable;
  in {
    org-plantuml-jar-path = "${plantuml}/lib/plantuml.jar";
    ob-mermaid-cli-path = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
    mermaid-mmdc-location = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
  };
  paths = let
    core = with pkgs; [
      fd # find file
      ripgrep # search
      pandoc # markdown, etc
      git
      languagetool # spelling
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
    ];
    org = with pkgs; [
      # graphs
      inkscape
      imagemagick
      graphviz-nox
      unstable.nodePackages.mermaid-cli
      ghostscript
      zip
    ];
    formatter = with pkgs; [
      nixpkgs-fmt
      pgformatter
      nodePackages.prettier
      unstable.alejandra
    ];
  in
    core ++ org ++ lsp ++ formatter;
}

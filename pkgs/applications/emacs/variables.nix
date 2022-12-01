{pkgs}: let
  inherit (pkgs) lib;
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
in {
  variables = let
    inherit (pkgs) plantuml unstable vscode-extensions nodePackages nodejs;
  in {
    org-plantuml-jar-path = "${plantuml}/lib/plantuml.jar";
    ob-mermaid-cli-path = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
    mermaid-mmdc-location = "${unstable.nodePackages.mermaid-cli}/bin/mmdc";
    lsp-eslint-server-command = [
      "${nodejs}/bin/node"
      "${pathOfExtension vscode-extensions.jmpunktPkgs.vscode-eslint}/server/out/eslintServer.js"
      "--stdio"
    ];
    prettier-js-command = "${nodePackages.prettier}/bin/prettier";
    nix-nixfmt-bin = "${unstable.alejandra}/bin/alejandra";
    eglot-languagetool-server-path = "${pkgs.ltex-ls}";
  };
  paths = let
    core = with pkgs; [
      fd # find file
      ripgrep # search
      pandoc # markdown, etc
      git
      gitAndTools.delta # git better diff
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
    ];
    org = with pkgs; [
      # graphs
      inkscape
      imagemagick
      graphviz-nox
      unstable.nodePackages.mermaid-cli
      ghostscript
      # odt-export
      unoconv
      zip
    ];
    formatter = with pkgs; [
      nixpkgs-fmt
      pgformatter
    ];
  in
    core ++ org ++ lsp ++ formatter;
}

{pkgs}: let
  inherit (pkgs) lib;
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";

  tree-sitter-grammars =
    pkgs.emacs-overlay.bundleTreeSitterGrammars
    (with pkgs.tree-sitter-grammars; [
      # NOTE: upstream
      tree-sitter-bash
      tree-sitter-c
      tree-sitter-c-sharp
      tree-sitter-cpp
      tree-sitter-css
      tree-sitter-dockerfile
      tree-sitter-java
      tree-sitter-python
      tree-sitter-javascript
      tree-sitter-json
      tree-sitter-tsx
      tree-sitter-typescript

      # NOTE: added by me
      tree-sitter-rust
      tree-sitter-nix
      tree-sitter-scss
      tree-sitter-html
      tree-sitter-toml
      tree-sitter-graphql
      tree-sitter-fish
    ]);
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
    treesit-extra-load-path = [ "${tree-sitter-grammars}/lib" ];
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
      nil
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

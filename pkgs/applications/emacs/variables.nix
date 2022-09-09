{pkgs}: let
  inherit (pkgs) lib;
  pathOfExtension = ext: "${ext}/share/vscode/extensions/${ext.vscodeExtUniqueId}";
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
    tree-sitter-load-path = ["${treeSitterGrammars}/bin"];
    # HACK: prevents tree sitter from compiling grammars
    tree-sitter-langs--testing = true;
    nix-nixfmt-bin = "${unstable.alejandra}/bin/alejandra";
    tree-sitter-indexer-bin = "${pkgs.tree-sitter-indexer}/bin/tree-sitter-indexer";
    tree-sitter-indexer-search-path = "${treeSitterGrammars}/bin";
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

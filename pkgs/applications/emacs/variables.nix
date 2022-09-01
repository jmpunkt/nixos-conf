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
  paths = with pkgs; [
    ccls
    # LSP
    rnix-lsp
    # LSP
    texlab
    # LSP
    yaml-language-server
    # LSP
    # haskellPackages.haskell-lsp
    # LSP
    nodePackages.typescript-language-server
    # LSP
    gitAndTools.delta
    # magit-delta
    inkscape
    # org-mode:graphs
    imagemagick
    # org-mode:graphs
    graphviz-nox
    # org-mode:graphs
    unstable.nodePackages.mermaid-cli
    # org-mode:graphs
    jre
    # required by plantuml
    ghostscript
    # LaTex EPS files
    unoconv
    # org-mode:odt
    zip
    # org-mode:odt
    languagetool
    # spelling
    nixpkgs-fmt
    # dev
    nodePackages.typescript
    # dev
    discount
    maven
    # dev
    cargo-outdated
    # dev
    git
    fd
    # affe
    ripgrep
    #affe
    jmpunkt.pythonToolchain
    # overlays/40-toolchains.nix
    jmpunkt.latex
    # custom latex suite
    # SQL
    pgformatter
    sqlite
  ];
}

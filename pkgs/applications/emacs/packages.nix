{pkgs}: epkgs:
with epkgs.melpaPackages;
  [
    # Core
    meow
    which-key
    hl-todo
    envrc
    smart-jump
    helpful
    xterm-color
    editorconfig
    reformatter
    gcmh
    parinfer-rust-mode
    biome
    transient-dwim
    rg
    # Org
    org-bullets
    ob-async
    ob-mermaid
    ob-graphql
    # PDF
    pdf-tools
    # Git
    magit
    diff-hl
    # UI
    all-the-icons
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
    tempel
    # Spelling
    flyspell-correct
    flymake-languagetool
    # RSS
    elfeed
    # Misc
    yaml-mode
    fish-mode
    graphql-mode
    mermaid-mode
    # Presentation
    markdown-mode
    graphviz-dot-mode
    # Programming
    rust-mode
    haskell-mode
    nix-ts-mode
    nix-mode
  ]
  ++ (with epkgs.elpaPackages; [undo-tree org])
  ++ (with epkgs.nongnuPackages; [eat])
  ++ (
    with epkgs.manualPackages;
      [
        jmpunktPkgs.dart-ts-mode
        jmpunktPkgs.ligature
        jmpunktPkgs.eglot-x
        jmpunktPkgs.eglot-booster
        jmpunktPkgs.typst-ts-mode
        jmpunktPkgs.ox-typst
        (jmpunktPkgs.nixosPaths (import ./variables.nix {inherit pkgs;}))
      ]
      ++ [
        treesit-grammars.with-all-grammars
      ]
  )

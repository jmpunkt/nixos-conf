{pkgs}: epkgs:
with epkgs.melpaPackages;
  [
    # Core
    avy
    meow
    which-key
    hl-todo
    envrc
    helpful
    xterm-color
    editorconfig
    reformatter
    gcmh
    biome
    casual-avy
    transient-dwim
    rg
    docker
    citre
    # Org
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
    languagetool
    # RSS
    elfeed
    # Misc
    yaml-mode
    graphql-mode
    mermaid-mode
    # Presentation
    ox-typst
    markdown-mode
    graphviz-dot-mode
    # Programming
    rust-mode
    nix-ts-mode
    nix-mode
  ]
  ++ (with epkgs.elpaPackages; [
    undo-tree
    org
    breadcrumb
  ])
  ++ (with epkgs.nongnuPackages; [eat])
  ++ (
    with epkgs.manualPackages;
      [
        jmpunktPkgs.eglot-x
        jmpunktPkgs.typst-ts-mode
        jmpunktPkgs.copilot
        jmpunktPkgs.xdg-appmenu
        (jmpunktPkgs.nixosPaths (import ./variables.nix {inherit pkgs;}))
      ]
      ++ [
        treesit-grammars.with-all-grammars
      ]
  )

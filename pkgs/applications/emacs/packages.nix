{pkgs}: epkgs:
with epkgs.melpaPackages;
  [
    esup
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
    doom-themes
    # Bibliography
    citar
    # SQL
    sqlformat
    # Search/Find
    consult
    orderless
    affe
    embark
    embark-consult
    marginalia
    cape
    tempel
    # Spelling
    flyspell-correct
    # RSS
    elfeed
    # Misc
    yaml-mode
    fish-mode
    nix-mode
    graphql-mode
    mermaid-mode
    # Presentation
    markdown-mode
    graphviz-dot-mode
    # Programming
    rust-mode
    haskell-mode
  ]
  ++ (with epkgs.elpaPackages; [undo-tree vertico org])
  ++ (
    with epkgs.manualPackages; [
      jmpunktPkgs.ligature
      jmpunktPkgs.eglot-x
      (jmpunktPkgs.nixosPaths (import ./variables.nix {inherit pkgs;}))
    ]
  )

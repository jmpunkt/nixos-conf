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
    yasnippet
    # Spelling
    flyspell-correct
    # RSS
    elfeed
    # LSP
    eglot
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
    prettier-js
  ]
  ++ (with epkgs.elpaPackages; [undo-tree seq vertico org])
  ++ (
    with epkgs.manualPackages; [
      jmpunktPkgs.ligature
      jmpunktPkgs.eglot-x
      (jmpunktPkgs.nixosPaths (import ./variables.nix {inherit pkgs;}))
    ]
  )

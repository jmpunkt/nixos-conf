;;; haskell.el --- Configuration for Haskell environment
;;; Commentary:

;;; Code:

(use-package haskell-mode
  :hook
  (haskell-mode . lsp)
  (haskell-mode . smartparens-mode))

(use-package flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

(provide 'haskell)
;;; haskell.el ends here

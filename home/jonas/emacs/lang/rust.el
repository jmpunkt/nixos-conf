;;; rust.el --- Configuration for Rust developement environment
;;; Commentary:

;;; Code:

(use-package rust-mode
  :hook
  (rust-mode . lsp)
  (rust-mode . smartparens-mode))

(use-package flycheck-rust
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'rust)
;;; rust.el ends here

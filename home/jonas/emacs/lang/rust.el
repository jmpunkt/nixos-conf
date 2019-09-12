;;; rust.el --- Configuration for Rust developement environment
;;; Commentary:

;;; Code:

(use-package rust-mode
  :after (lsp smartparens)
  :hook ((rust-mode . lsp)
         (rust-mode . smartparens-mode)))

(use-package flycheck-rust
  :after flycheck
  :hook (flycheck-mode . flycheck-rust-setup))

(provide 'rust)
;;; rust.el ends here

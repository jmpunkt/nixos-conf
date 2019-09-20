;;; python.el --- Configuration for Python developement environment
;;; Commentary:

;;; Code:
(use-package python
  :after (lsp-mode smartparens)
  :mode ("\\.py\\'" . python-mode)

  :hook
  (python-mode . lsp)
  (python-mode . smartparens-mode)

  :config
  (setq python-indent-offset 4))

(provide 'python)
;;; python.el ends here

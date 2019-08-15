;;; python.el --- Configuration for Python developement environment
;;; Commentary:

;;; Code:
(use-package python
  :mode
  ("\\.py\\'" . python-mode)

  :hook
  (python-mode . lsp)
  (python-mode . smartparens-mode)

  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 4))

(provide 'python)
;;; python.el ends here

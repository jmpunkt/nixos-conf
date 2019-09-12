;;; init-rst.el --- Configuration for RST documents
;;; Commentary:

;;; Code:
(use-package rst
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

(provide 'init-rst)
;;; init-rst.el ends here

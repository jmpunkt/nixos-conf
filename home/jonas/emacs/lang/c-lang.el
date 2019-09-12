;;; c-lang.el --- Configuration for C/C++ developement environment
;;; Commentary:

;;; Code:
(use-package irony
  :hook ((c-mode . irony-mode)
         (objc-mode . irony-mode)
         (c++-mode .irony-mode)))

(use-package flycheck-irony
  :after (flycheck irony)
  :defer t)

(provide 'c-lang)
;;; c-lang.el ends here

;;; web.el --- Configuration for Web developement environment
;;; Commentary:

;;; Code:



(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . smartparens-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode -1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        company-tooltip-align-annotations t))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package js2-mode
  :ensure t
  :after smartparens
  :mode (("\\.js\\'" . js2-mode))
  :hook
  (js2-mode . smartparens-mode)
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2))

(use-package web-mode
  :ensure t
  :after smartparens
  :mode
  ("\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.tsx\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'"
   "\\.html?\\'")
  :hook
  (web-mode . smartparens-mode)

  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2

        web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t)

  :hook ((web-mode . (lambda ()
                       (setq web-mode-code-indent-offset 2)
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide-mode)))))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'"))
        web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))))

(provide 'web)
;;; web.el ends here

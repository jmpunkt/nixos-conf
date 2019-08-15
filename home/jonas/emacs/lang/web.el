;;; web.el --- Configuration for Web developement environment
;;; Commentary:

;;; Code:
(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.html?\\'"
   "\\.tsx\\'" "\\.jsx\\'")

  :hook
  (web-mode . lsp)
  (web-mode . smartparens-mode)

  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2

        web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t)

  :config
  ;; Template
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))))

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'"))

(provide 'web)
;;; web.el ends here

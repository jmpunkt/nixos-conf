;;; globals.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:
(use-package all-the-icons
  :init (setq inhibit-compacting-font-caches t))

(use-package doom-themes
  :init (doom-themes-treemacs-config))

(use-package doom-modeline
  :init
  (setq doom-modeline-icon t
        doom-modeline-lsp t))

(use-package emacs
  :init
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default show-paren-delay 0)
  (setq-default show-trailing-whitespace t)
  (setq indent-line-function 'insert-tab)

  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq column-number-mode t)

  (show-paren-mode 1)
  (global-linum-mode t)
  (global-hl-line-mode +1)
  (global-prettify-symbols-mode t)

  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (global-eldoc-mode -1)
  (defvar eldoc-in-minibuffer-mode nil)

  (global-set-key [f1] 'shell)

  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight normal :height 113 :width normal)))))
  (custom-set-variables
   '(custom-safe-themes
     (quote
      ("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default))))

  (load-theme 'doom-vibrant))
(provide 'globals)
;;; globals.el ends here

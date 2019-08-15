;;; ui.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:
(require 'all-the-icons)
(setq inhibit-compacting-font-caches t)

(require 'doom-themes)
(doom-themes-treemacs-config)
(require 'doom-modeline)
(setq doom-modeline-icon t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq-default show-trailing-whitespace t)
(show-paren-mode 1)
(setq-default show-paren-delay 0)
(global-linum-mode t)
(global-hl-line-mode +1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-eldoc-mode -1)
(setq column-number-mode t)
(defvar eldoc-in-minibuffer-mode nil)
(global-set-key [f1] 'shell)

(custom-set-faces
;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight normal :height 113 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default))))

;; (set-variable flyspell-issue-message-flag nil)

(load-theme 'doom-vibrant)

(provide 'ui)
;;; ui.el ends here

;;; core.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:

(use-package evil
  :config
  (evil-mode 1))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package yasnippet
  :ensure t
  :config
  :after company
  (add-to-list 'company-backends '(company-yasnippet))
  (yas-global-mode))

(use-package yasnippet-snippets :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20)

  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)

  (setq company-backends '((company-files)))

  (global-company-mode t))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-inhibit-message t
        lsp-print-performance nil
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil
        lsp-document-sync-method 'incremental))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point))

(use-package company-lsp
  :commands company-lsp
  :after  company
  :ensure t
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :after direnv
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3
        flycheck-executable-find
        (lambda (cmd) (direnv-update-environment default-directory)(executable-find cmd))))

(use-package flyspell
  :ensure t
  :config

  ;; Set programms
  (setq-default ispell-program-name "aspell")
  (setq-default ispell-list-command "--list")

  ;; Refresh flyspell after directory change
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))

(use-package flyspell-correct-ivy
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package smartparens
  :ensure t
  :config

  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)

  (sp-pair "'" nil :actions :rem))

(provide 'core)
;;; core.el ends here

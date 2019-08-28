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

(use-package which-key
  :config
  (which-key-mode))

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
        lsp-document-sync-method 'incremental
        lsp-enable-snipped t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable nil))

(use-package company-lsp
  :commands company-lsp
  :after company
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
  :bind ((:map flyspell-mode-map
               ("C-x c" . flyspell-correct-at-point)))
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

(use-package projectile
  :ensure t
  :config

  ;; Global configuration
  (setq projectile-switch-project-action 'neotree-projectile-action
        projectile-enable-caching t
        projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-project-function 'file-remote-p)

  ;; Defining some helpers
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    ;; This requires a snapshot version of Projectile.
    (projectile-run-shell))

  (def-projectile-commander-method ?\C-?
    "Go back to project selection."
    (projectile-switch-project))

  ;; Keys
  (setq projectile-keymap-prefix (kbd "C-x p"))

  ;; Activate globally
  (projectile-mode))

(use-package ediff
  :config
  (autoload 'diff-mode "diff-mode" "Diff major mode" t)
  (setq diff-switches "-u"
        ediff-auto-refine-limit (* 2 14000)
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function
        (lambda (&optional arg)
          (if (> (frame-width) 160)
              (split-window-horizontally arg)
            (split-window-vertically arg)))))
(defun diff-region ()
  "Select a region to compare"
  (interactive)
  (when (use-region-p) ; there is a region
    (let (buf)
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (region-beginning) (region-end)))
    )
  (message "Now select other region to compare and run `diff-region-now`"))

(defun diff-region-now ()
  "Compare current region with region already selected by `diff-region`"
  (interactive)
  (when (use-region-p)
    (let (bufa bufb)
      (setq bufa (get-buffer-create "*Diff-regionA*"))
      (setq bufb (get-buffer-create "*Diff-regionB*"))
      (save-current-buffer
        (set-buffer bufb)
        (erase-buffer))
      (append-to-buffer bufb (region-beginning) (region-end))
      (ediff-buffers bufa bufb))))

(use-package treemacs
  :ensure t
  :after hl-line-mode
  :config
  (setq treemacs-follow-after-init          t
        treemacs-width                      35
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-collapse-dirs              3
        treemacs-silent-refresh             nil
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil
        treemacs-is-never-other-window      nil
        treemacs-goto-tag-strategy          'refetch-index)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :bind
  (:map global-map
        ([f8]        . treemacs-toggle)))

(use-package treemacs-projectile
  :ensure t
  :after treemacs
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(provide 'core)
;;; core.el ends here

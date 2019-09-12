;;; core.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:
(use-package evil
  :config
  (evil-mode 1))

(use-package direnv
  :config
  (direnv-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package flycheck
  :init
  (global-flycheck-mode t)
  ;; Hydra Flycheck
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  :after direnv
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3
        flycheck-executable-find
        (lambda (cmd) (direnv-update-environment default-directory)(executable-find cmd))))

(use-package flycheck-package)

(use-package flyspell
  :bind ((:map flyspell-mode-map
               ("C-x c" . flyspell-correct-at-point)))
  :hook ((text-mode . flyspell-mode))
  :config

  ;; Set programms
  (setq-default ispell-program-name "aspell"
                ispell-list-command "--list")

  ;; Refresh flyspell after directory change
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  (sp-pair "'" nil :actions :rem))

(use-package projectile
  :after ivy
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action
        projectile-enable-caching t
        projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-project-function 'file-remote-p
        projectile-completion-system 'ivy)
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
  "Select a region to compare."
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
  "Compare current region with region already selected by `diff-region`."
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
  :after hl-line-mode
  :bind (([f8] . treemacs))
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
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after treemacs
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(provide 'core)
;;; core.el ends here

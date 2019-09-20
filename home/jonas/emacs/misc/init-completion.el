;;; init-completion.el --- Intilized completion plugins
;;; Commentary:

;;; Code:
(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :after (counsel projectile)
  :bind (("C-x m" . hydra-projectile/body))
  :config
  (defhydra hydra-projectile (:exit t :hint nil)
    "
  Projectile^^        Buffers^^           Find^^^^            Search^^
-------------------------------------------------------------------------------------
  [_q_] quit          [_b_] list          [_d_] directory     [_r_] replace
  [_i_] reset cache   [_k_] kill all      [_D_] root          [_R_] regexp replace
  ^^                  [_S_] save all      [_f_] file          [_s_] search
  ^^                  ^^                  [_p_] project^^
  ^^                  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("b" counsel-projectile-switch-to-buffer)
    ("d" counsel-projectile-find-dir)
    ("D" projectile-dired)
    ("f" counsel-projectile-find-file)
    ("i" projectile-invalidate-cache :color red)
    ("k" projectile-kill-buffers)
    ("p" counsel-projectile-switch-project)
    ("r" projectile-replace)
    ("R" projectile-replace-regexp)
    ("s" counsel-projectile-rg)
    ("S" projectile-save-project-buffers)))

(use-package company
  :hook ((emacs-lisp-mode . (lambda ()
                              (add-to-list
                               (make-local-variable 'company-backends)
                               '(company-elisp)))))
  :bind (:map company-active-map
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-limit 20)

  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-files)
  (global-company-mode t))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package yasnippet
  :after company
  :config
  (add-to-list 'company-backends '(company-yasnippet))
  (yas-global-mode))

(use-package yasnippet-snippets)

(provide 'init-completion)
;;; init-completion.el ends here

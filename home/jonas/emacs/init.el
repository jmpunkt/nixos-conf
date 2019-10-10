;;; init.el --- Initilizes Emacs
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)
(eval-when-compile (require 'use-package))

;;; * Theme
(use-package all-the-icons
  :init (setq inhibit-compacting-font-caches t))

(use-package doom-themes
  :init (doom-themes-treemacs-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t
        doom-modeline-lsp t))

;;; * Emacs
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
  (setq eldoc-in-minibuffer-mode nil)

  (global-set-key [f1] 'eshell)

  (custom-set-variables
   '(custom-safe-themes
     (quote
      ("43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default))))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight normal :height 113 :width normal)))))

  (load-theme 'doom-vibrant))

;;; * Core Packages

;;;; * Evil
(use-package evil
  :config
  (evil-mode 1))

;;;; * DirEnv
(use-package direnv
  :config
  (direnv-mode))

;;;; * Which-Key
(use-package which-key
  :config
  (which-key-mode))

;;;; * Outshine
(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;;; * Hydra

(use-package hydra
  :after smartparens
  :bind (:map global-map
         ("C-x p" . hydra-smartparens/body)
         ("C-x w" . hydra-window/body))
  :init
  (defhydra hydra-window ()
    "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete
"
    ("h" windmove-left )
    ("j" windmove-down )
    ("k" windmove-up )
    ("l" windmove-right )
    ("q" hydra-move-splitter-left)
    ("w" hydra-move-splitter-down)
    ("e" hydra-move-splitter-up)
    ("r" hydra-move-splitter-right)
    ("b" helm-mini)
    ("f" helm-find-files)
    ("F" follow-mode)
    ("a" (lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)))
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)))
    ("s" (lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (lambda ()
           (interactive)
           (ace-window 16)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
    ("o" delete-other-windows)
    ("i" ace-maximize-window)
    ("z" (progn
           (winner-undo)
           (setq this-command 'winner-undo)))
    ("Z" winner-redo)
    ("SPC" nil))

  ;; Hydra Smartparens
  (defhydra hydra-smartparens (:hint nil)
    "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
    ;; Moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp)
    ;; Slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)
    ;; Wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square)
    ;; Sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)
    ;; Destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp)

    ("q" nil)
    ("g" nil)))

;;;; * Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode t)
  ;; Hydra Flycheck
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter "Filter")
    ("j"  flycheck-next-error "Next")
    ("k"  flycheck-previous-error "Previous")
    ("gg" flycheck-first-error "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  :after direnv
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3
        flycheck-executable-find
        ;; Uses direnv environment
        (lambda (cmd) (direnv-update-environment default-directory)(executable-find cmd))))

;;;; * Flyspell
(use-package flyspell
  :bind (:map flyspell-mode-map
               ("C-x c" . flyspell-correct-at-point))
  :hook (text-mode . flyspell-mode)
  :config
  (setq-default ispell-program-name "aspell"
                ispell-list-command "--list")
  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

;;;; * Smartparens
(use-package smartparens
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  (sp-pair "'" nil :actions :rem))

;;;; * Projectile
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

;;;; * Diff
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

;;;; * Treemacs
(use-package treemacs
  :bind (:map global-map
        ([f8] . treemacs)
        ("C-c f" . treemacs-select-window))
  :hook (treemacs-mode . (lambda () (linum-mode 0)))
  :config
  (setq treemacs-follow-after-init t
        treemacs-width 35
        treemacs-indentation 2
        treemacs-git-integration t
        treemacs-collapse-dirs 3
        treemacs-silent-refresh nil
        treemacs-change-root-without-asking nil
        treemacs-sorting  'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-never-persist nil
        treemacs-is-never-other-window nil
        treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after treemacs
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

;;;; * Language Server (LSP)
(use-package lsp-mode
  :after hydra
  ;; :bind (:map lsp-mode-map
  ;;             ([f6] . hydra-lsp/body))
  :bind (:map global-map
              ([f6] . hydra-lsp/body))
  :init
  (defhydra hydra-lsp (:exit t :hint nil)
    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))
  :config
  (setq lsp-inhibit-message nil
        lsp-print-performance nil
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-eldoc-prefer-signature-help nil
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-highlight-symbol-at-point nil
        lsp-document-sync-method 'incremental
        lsp-enable-snipped t
        lsp-prefer-flymake nil))

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
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

;;; * Completion

;;;; * Ivy
(use-package ivy
  :demand t
  :bind (:map global-map
              ("C-s" . swiper))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t))

(use-package counsel
  :bind (:map global-map
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :after (counsel projectile)
  :bind (:map global-map
         ("C-x m" . hydra-projectile/body))
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

;;;; * Company
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
  :init (global-company-mode t)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-limit 20)

  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-capf))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

;;;; * Snippets
(use-package yasnippet
  :after company
  :config
  (add-to-list 'company-backends '(company-yasnippet))
  (yas-global-mode))

(use-package yasnippet-snippets)

;;; * Git

(use-package magit
  :config
  ;; Ignore recent commit
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpulled-from-upstream
          magit-insert-unpulled-from-pushremote
          magit-insert-unpushed-to-upstream
          magit-insert-unpushed-to-pushremote)
        pretty-magit-alist nil
        pretty-magit-prompt nil)

  (defhydra hydra-magit (:color blue :hint nil)
    "
  ^Magit^             ^Do^
-----------------------------------------------
  [_q_] quit          [_b_] blame
  ^^                  [_c_] clone
  ^^                  [_i_] init
  ^^                  [_s_] status
  ^^                  ^^
"
    ("q" nil)
    ("b" magit-blame)
    ("c" magit-clone)
    ("i" magit-init)
    ("s" magit-status))

  ;; Opening repo externally
  (defun parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))
  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (parse-url url))
        (message "opening repo %s" url))))

  (add-hook 'magit-mode-hook
            (lambda ()
              (local-set-key (kbd "o") 'magit-open-repo))))

(use-package evil-magit)

;;; * Eshell
(use-package eshell
  :after ansi-color
  :config
  (defun eshell-handle-ansi-color ()
    (ansi-color-apply-on-region eshell-last-output-start
                                eshell-last-output-end))
  (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

;;; * Configuration Files

;;;; * JSON
(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

;;;; * TOML
(use-package toml-mode
  :mode ("\\.toml\\'"))

;;;; * YAML
(use-package yaml-mode
  :mode ("\\.yaml$" "\\.yml\\'"))

;;;; * Meson
(use-package meson-mode
  :init
  (setq meson-indent-basic 4))

;;;; * GraphQL
(use-package graphql-mode
  :mode ("\\.graphql\\'"))

;;; * Text Files

;;;; * reStructuredText
(use-package rst
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;;;; * Markdown
(use-package markdown-mode
  :after (flyspell hydra)
  :mode
  ("INSTALL\\'"
   "CONTRIBUTORS\\'"
   "LICENSE\\'"
   "README\\'"
   "\\.markdown\\'"
   "\\.md\\'")
  :hook (markdown-mode . flyspell-mode)
  :bind (:map markdown-mode-map
              ([f6] . hydra-markdown-mode/body))
  :init
  (defhydra hydra-markdown-mode (:hint nil)
    "
   Formatting^^           Headings^^         References^^       Other
-------------------------------------------------------------------------------------
   [_s_] bold             [_h_] automatic    [_L_] link         [_m_] insert item
   [_e_] italic           [_1_] h1           [_U_] uri          [_l_] promote
   [_b_] blockquote       [_2_] h2           [_F_] footnote     [_r_] demote
   [_p_] pre-formatted    [_3_] h3           [_W_] wiki-link    [_u_] move up
   [_c_] code             [_4_] h4                              [_d_] move down
    "
    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)

    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)

    ("m" markdown-insert-list-item)

    ("l" markdown-promote)
    ("r" markdown-demote)
    ("d" markdown-move-down)
    ("u" markdown-move-up)

    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)))

(use-package markdown-mode+
  :after markdown-mode)

;;; * Programming Languages

;;;; * C/C++
(use-package irony
  :hook ((c-mode . irony-mode)
         (objc-mode . irony-mode)
         (c++-mode .irony-mode)))

(use-package flycheck-irony
  :after (flycheck irony))

;;;; * Elm
(use-package elm-mode
  :after company
  :config
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))

;;;; * Haskell
(use-package haskell-mode
  :after (lsp-mode smartparens)
  :hook ((haskell-mode . lsp)
         (haskell-mode . smartparens-mode)))

(use-package flycheck-haskell
  :after (flycheck)
  :hook (haskell-mode . flycheck-haskell-setup))

;;;; * Lua
(use-package lua-mode)

(use-package company-lua
  :after company)

;;;; * Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;;;; * Python
(use-package python
  :after (lsp-mode smartparens)
  :mode ("\\.py\\'" . python-mode)

  :hook
  (python-mode . lsp)
  (python-mode . smartparens-mode)

  :config
  (setq python-indent-offset 4))

;;;; * Rust
(use-package rust-mode
  :after (lsp-mode smartparens)
  :hook ((rust-mode . lsp)
         (rust-mode . smartparens-mode)))

(use-package flycheck-rust
  :after flycheck
  :hook (flycheck-mode . flycheck-rust-setup))

;;;; * Fish
(use-package fish-mode
  :mode "\\.fish\\'")

;;;; * Typescript
(use-package tide
  :after (flycheck company)
  :init (defun setup-tide-mode ()
          (interactive)
          (tide-setup)
          (flycheck-mode +1)
          (eldoc-mode -1)
          (tide-hl-identifier-mode +1)
          (company-mode +1)
          (setq flycheck-check-syntax-automatically '(save mode-enabled)
                company-tooltip-align-annotations t)))

(use-package typescript-mode
  :after smartparens
  :mode ("\\.ts\\'")
  :hook ((typescript-mode . smartparens-mode)
         (typescript-mode . setup-tide-mode)))

;;;; * Javascript
(use-package js2-mode
  :after smartparens
  :mode (("\\.js\\'" . js2-mode))
  :hook ((js2-mode . smartparens-mode))
  :interpreter (("node" . js2-mode))
  :config
  (setq js2-basic-offset 4))

;;;; * HTML/JSX/RSX
(use-package web-mode
  :after (smartparens flycheck tide)
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
  ((web-mode . smartparens-mode)
   (web-mode . (lambda ()
                 (when (and
                        buffer-file-name
                        (equal "tsx" (file-name-extension buffer-file-name)))
                   (setup-tide-mode)))))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2

        web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'"))
        web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))))

;;; * PDF

(use-package pdf-tools
  :after hydra
  :hook
  (pdf-view-mode . (lambda ()
                     (pdf-misc-size-indication-minor-mode)
                     (pdf-links-minor-mode)
                     (pdf-isearch-minor-mode)
                     (cua-mode 0)))
  :bind
  (:map pdf-view-mode-map
         ("/" . hydra-pdftools/body)
         ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
         ("g"  . pdf-view-first-page)
         ("G"  . pdf-view-last-page)
         ("l"  . image-forward-hscroll)
         ("h"  . image-backward-hscroll)
         ("j"  . pdf-view-next-page)
         ("k"  . pdf-view-previous-page)
         ("e"  . pdf-view-goto-page)
         ("u"  . pdf-view-revert-buffer)
         ("al" . pdf-annot-list-annotations)
         ("ad" . pdf-annot-delete)
         ("aa" . pdf-annot-attachment-dired)
         ("am" . pdf-annot-add-markup-annotation)
         ("at" . pdf-annot-add-text-annotation)
         ("y"  . pdf-view-kill-ring-save)
         ("i"  . pdf-misc-display-metadata)
         ("s"  . pdf-occur)
         ("b"  . pdf-view-set-slice-from-bounding-box)
         ("r"  . pdf-view-reset-slice))
  :config
  (pdf-tools-install t t t)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-to-list 'auto-mode-alist (cons "\\.pdf\\'" 'pdf-view-mode))

  (defhydra hydra-pdftools (:color blue :hint nil)
    "
      PDF tools

   Move  History   Scale/Fit                  Annotations     Search/Link     Do
------------------------------------------------------------------------------------------------
     ^^_g_^^      _B_    ^↧^    _+_    ^ ^     _al_: list    _s_: search    _u_: revert buffer
     ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   _am_: markup  _o_: outline   _i_: info
     ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     _at_: text    _F_: link      _d_: dark mode
     ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   _ad_: delete  _f_: search link
_h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     _aa_: dired
     ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   _y_:  yank
     ^^_n_^^      ^ ^  _r_eset slice box
     ^^^↓^^^
     ^^_G_^^
"
          ("\\" hydra-master/body "back")
          ("<ESC>" nil "quit")
          ("al" pdf-annot-list-annotations)
          ("ad" pdf-annot-delete)
          ("aa" pdf-annot-attachment-dired)
          ("am" pdf-annot-add-markup-annotation)
          ("at" pdf-annot-add-text-annotation)
          ("y"  pdf-view-kill-ring-save)
          ("+" pdf-view-enlarge :color red)
          ("-" pdf-view-shrink :color red)
          ("0" pdf-view-scale-reset)
          ("H" pdf-view-fit-height-to-window)
          ("W" pdf-view-fit-width-to-window)
          ("P" pdf-view-fit-page-to-window)
          ("n" pdf-view-next-page-command :color red)
          ("p" pdf-view-previous-page-command :color red)
          ("d" pdf-view-dark-minor-mode)
          ("b" pdf-view-set-slice-from-bounding-box)
          ("r" pdf-view-reset-slice)
          ("g" pdf-view-first-page)
          ("G" pdf-view-last-page)
          ("e" pdf-view-goto-page)
          ("o" pdf-outline)
          ("s" pdf-occur)
          ("i" pdf-misc-display-metadata)
          ("u" pdf-view-revert-buffer)
          ("F" pdf-links-action-perfom)
          ("f" pdf-links-isearch-link)
          ("B" pdf-history-backward :color red)
          ("N" pdf-history-forward :color red)
          ("l" image-forward-hscroll :color red)
          ("h" image-backward-hscroll :color red)))

;;; * Org

(use-package academic-phrases)

(use-package langtool
  :init
  ;; for NixOS use languagetool-commandline?
  (setq langtool-bin "languagetool-commandline"))

(use-package org
  :after (flyspell flycheck)
  :init
  (setq papers-dir (expand-file-name "~/Documents/papers/")
        papers-pdfs (concat papers-dir "lib/")
        papers-notes (concat papers-dir "index.org")
        papers-refs (concat papers-dir "index.bib")
        org-highlight-latex-and-related '(latex)
        org-bullets-bullet-list '("●" "○" "✸" "✿")
        org-ellipsis "…"
        org-catch-invisible-edits 'smart)
  (defun disable-fylcheck-in-org-src-block ()
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :hook ((org-mode . flyspell-mode)
         (org-src-mode . disable-fylcheck-in-org-src-block))
  :config
  ;; Sets the buffer name of org source blocks properly
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name)))
      ad-do-it
      (setq buffer-file-name file-name)))
  (setq org-agenda-files '("~/Documents/org/"))
  (setq org-capture-templates
        '(("t" "TODO" entry (file+headline (concat org-agenda-files "todo.org") "ToDo")
           "* TODO %? %^G \n  %U" :empty-lines 1)
          ("d" "Deadline" entry (file+headline (concat org-agenda-files "todo.org") "ToDo")
           "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
          ("p" "Priority" entry (file+headline (concat org-agenda-files "todo.org") "ToDo")
           "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
          ("a" "Appointment" entry (file+headline (concat org-agenda-files "calendar.org") "Event")
           "* %? %^G \n  %^t")
          ("l" "Link" entry (file+headline (concat org-agenda-files "notes.org") "Link")
           "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
          ("n" "Note" entry (file+headline (concat org-agenda-files "notes.org") "Notes")
           "* %? %^G\n%U" :empty-lines 1)
          ("j" "Journal" entry (file+datetree (concat org-agenda-files "journal.org") "Journal")
           "* %? %^G\nEntered on %U\n")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (js . t)
     (python . t)
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (gnuplot . t)
     (sql . t)
     (dot . t))))

(use-package toc-org
  :after (markdown-mode org-mode)
  :hook
  ((org-mode . toc-org-mode)
   (markdown-mode . toc-org-mode))
  :bind (:map markdown-mode-map
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point)))

(use-package org-ref
  :after org
  :config
  ;; (setq reftex-default-bibliography (list papers-refs))
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes papers-notes
        org-ref-default-bibliography (list papers-refs)
        org-ref-pdf-directory papers-pdfs))

(use-package org-noter
  :after org
  :commands org-noter
  :config
  (setq org-noter-default-notes-file-names '("index-org")
	  org-noter-notes-search-path (list papers-dir)
	  org-noter-auto-save-last-location t
	  org-noter-doc-split-fraction '(0.8 . 0.8)
	  org-noter-always-create-frame nil
	  org-noter-insert-note-no-questions t
	  org-noter-notes-window-location 'vertical-split))

(use-package ivy-bibtex
  :after (ivy org)
  :config
  (setq bibtex-completion-bibliography papers-refs
        bibtex-completion-library-path papers-pdfs
        bibtex-completion-notes-path papers-notes))

(use-package interleave
  :after org
  :bind (:map global-map
              ("C-x i" . interleave-mode))
  :config
  (setq interleave-split-direction 'horizontal
        interleave-split-lines 20
        interleave-disable-narrowing t))

;;; * LaTeX

(use-package tex-site
  :after (auctex tex latex)
  :hook
  ((LaTeX-mode . turn-off-auto-fill)
   (LaTeX-mode . (lambda () (TeX-fold-mode t)))
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . outline-minor-mode))
  :config
  ;; Spelling
  (setq ispell-tex-skip-alists
        '((
           ;;("%\\[" . "%\\]") ; AMStex block comment...
           ;; All the standard LaTeX keywords from L. Lamport's guide:
           ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly
           ;; \input, \label, \nocite, \rule (in ispell - rest included here)
           ("\\\\addcontentsline"              ispell-tex-arg-end 2)
           ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
           ("\\\\\\([aA]lph\\|arabic\\)"   ispell-tex-arg-end)
           ("\\\\author"                         ispell-tex-arg-end)
           ;; New regexps here --- kjh
           ("\\\\\\(text\\|paren\\)cite" ispell-tex-arg-end)
           ("\\\\cite\\(t\\|p\\|year\\|yearpar\\)" ispell-tex-arg-end)
           ("\\\\bibliographystyle"                ispell-tex-arg-end)
           ("\\\\makebox"                  ispell-tex-arg-end 0)
           ("\\\\e?psfig"                  ispell-tex-arg-end)
           ("\\\\document\\(class\\|style\\)" .
            "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
          (
           ;; delimited with \begin.  In ispell: displaymath, eqnarray,
           ;; eqnarray*, equation, minipage, picture, tabular,
           ;; tabular* (ispell)
           ("\\(figure\\|table\\)\\*?"     ispell-tex-arg-end 0)
           ("\\(equation\\|eqnarray\\)\\*?"     ispell-tex-arg-end 0)
           ("list"                                 ispell-tex-arg-end 2)
           ("program" . "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
           ("verbatim\\*?"."\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")
           ("lstlisting\\*?"."\\\\end[ \t\n]*{[ \t\n]*lstlisting\\*?[ \t\n]*}"))))

  (TeX-global-PDF-mode 1)
  (setq-default TeX-master nil)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-open-quote "\enquote{"
        TeX-close-quote "}"
        LaTeX-indent-level 4
        LaTeX-item-indent 0
        TeX-brace-indent-level 4
        TeX-newline-function 'newline-and-indent
        TeX-source-correlate-method 'synctex)

  ;; Minor helpers for comment and quotes
  (add-to-list 'LaTeX-verbatim-environments "comment")
  (define-key LaTeX-mode-map (kbd "C-c C-=") 'align-current))

(use-package bibtex
  :config
  (setq bibtex-align-at-equal-sign t
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an")
        bibtex-autokey-titleword-length 100
        bibtex-autokey-titlewords 1))

(use-package company-auctex
  :hook
  (latex-mode . (company-auctex-init)))

(use-package company-bibtex
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex)))))

(use-package company-reftex
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations)))))

(use-package company-math
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode)))))

(use-package auctex-latexmk
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :config
  (setq reftex-save-parse-info t
        reftex-enable-partial-scans t
        reftex-use-multiple-selection-buffers t
        reftex-plug-into-AUCTeX t
        reftex-vref-is-default t
        reftex-cite-format
        '((?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?x . "[]{%l}")
          (?X . "{%l}"))
        font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{")
          ("citetitle" "[{")
          ("citetitles" "[{")
          ("headlessfullcite" "[{"))

        reftex-cite-prompt-optional-args nil
        reftex-cite-cleanup-optional-args t))

(use-package latex-math-preview
  :hook (LaTeX-mode-hook . LaTeX-preview-setup)
  :config
  (autoload 'LaTeX-preview-setup "preview")
  (setq preview-scale-function 1.2))


;;; * -- End
(provide 'init)
;;; init.el ends here

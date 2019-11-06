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
  :bind (:map global-map
              ([f1] . eshell))
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
  :bind (:map global-map
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
    ("b" switch-to-buffer)
    ("f" counsel-find-file)
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
    ("SPC" nil)))

;;;; * Flycheck
(use-package flycheck
  :requires direnv
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
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3
        flycheck-executable-find
        ;; Uses direnv environment
        (lambda (cmd) (direnv-update-environment default-directory)(executable-find cmd))))

;;;; * Spelling
(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-c s" . hydra-spelling/body))
  :hook (text-mode . flyspell-mode)
  :init
  (defhydra hydra-spelling (:color blue)
    "^
  ^Spelling^          ^Errors^            ^Checker^
------------------------------------------------------------
  [_q_] quit          [_<_] previous      [_c_] correction
  ^^                  [_>_] next          [_d_] dictionary
  ^^                  [_f_] check         [_m_] mode
  ^^                  ^^                  ^^"
    ("q" nil)
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" flyspell-correct-at-point)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer)
    ("m" flyspell-mode))
  :config
  (setq-default ispell-program-name "aspell"
                ispell-list-command "--list")
  (defun flyspell-buffer-after-pdict-save (&rest _) (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))

(use-package flyspell-correct-ivy
  :requires (flyspell ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package langtool
  :bind (:map global-map
              ("C-c l" . hydra-langtool/body))
  :init
  (defhydra hydra-langtool (:color blue)
    "
  ^
  ^Spelling^          ^Errors^            ^Checker^
------------------------------------------------------------
  [_q_] quit          [_<_] previous      [_c_] correction
  ^^                  [_>_] next          [_s_] show error
  ^^                  [_f_] check         ^^
  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("<" langtool-goto-previous-error :color pink)
    (">" langtool-goto-next-error :color pink)
    ("c" langtool-correct-buffer)
    ("s" langtool-show-message-at-point)
    ("f" langtool-check))
  :config
  ;; for NixOS use languagetool-commandline?
  (setq langtool-bin "languagetool-commandline"))

;;;; * Smartparens
(use-package smartparens
  :hook (eval-expression-minibuffer-setup . smartparens-mode)
  :bind (:map global-map
              ("C-c p" . hydra-smartparens/body))
  :init
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
    ("g" nil))
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem))

;;;; * Projectile
(use-package projectile
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action
        projectile-enable-caching t
        projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-project-function 'file-remote-p
        projectile-completion-system 'ivy)
  (projectile-mode))

(use-package org-projectile
  :requires (org projectile)
  :bind (:map global-map
              ("C-c n p" . org-projectile-project-todo-completing-read)
              ("C-c c" . org-capture))
  :config
  (setq org-projectile-projects-file (expand-file-name "projectile.org" org-remote-dir)
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

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
        treemacs-collapse-dirs 3
        treemacs-silent-refresh nil
        treemacs-sorting  'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-is-never-other-window nil
        treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :requires (treemacs projectile))

;;;; * Language Server (LSP)
(use-package lsp-mode
  :commands lsp
  :bind (:map lsp-mode-map
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
    ("M-r" lsp-workspace-restart)
    ("S" lsp-workspace-shutdown))
  :config
  (setq lsp-print-performance nil
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-eldoc-prefer-signature-help nil
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-snippet t
        lsp-prefer-flymake nil))

(use-package lsp-treemacs
  :requires (lsp-mode treemacs))

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
  :requires (company lsp-mode)
  :config
  (setq-local company-backends (append '(company-lsp) company-backends))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

;;;; * Completion
;;;;; * Ivy
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

(use-package ivy-bibtex
  :requires (ivy org org-ref bibtex)
  :ensure org-ref
  :config
  (setq bibtex-completion-bibliography papers-refs
        bibtex-completion-library-path papers-pdfs
        bibtex-completion-notes-path papers-notes))

(use-package counsel
  :bind (:map global-map
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :requires (counsel projectile)
  :bind (:map projectile-mode-map
              ("C-c p" . hydra-projectile/body))
  :init
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

;;;;; * Company
(use-package company
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local
                              company-backends
                              (append '(company-elisp) company-backends))))
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

  (setq company-backends
        '(company-bbdb company-dabbrev-code company-files company-capf)))

(use-package company-quickhelp
  :requires company
  :config
  (company-quickhelp-mode 1))

;;;;; * Snippets
(use-package yasnippet
  :requires company
  :config
  (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode))

(use-package yasnippet-snippets)

;;;; * Git
(use-package magit
  :bind (:map magit-mode-map
              ("C-o" . magit-open-repo))
  :config
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
          magit-insert-unpushed-to-pushremote))
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
        (message "opening repo %s" url)))))

(use-package evil-magit)

;;;; * Eshell
(use-package eshell
  :requires ansi-color
  :config
  (defun eshell-handle-ansi-color ()
    (ansi-color-apply-on-region eshell-last-output-start
                                eshell-last-output-end))
  (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

;;;; * Org
(use-package academic-phrases)

(use-package org
  :requires (flyspell flycheck)
  :defines (org-remote-dir)
  :init
  (defvar org-remote-dir (expand-file-name "~/Dropbox"))
  (defvar org-agenda-dir (expand-file-name "agenda" org-remote-dir))
  (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "…"
        org-catch-invisible-edits 'smart)
  :hook ((org-mode . flyspell-mode)
         (org-mode . (lambda () (setq-local tab-width 2)))
         (org-src-mode . (lambda ()
                           (setq-local
                            flycheck-disabled-checkers
                            '(emacs-lisp-checkdoc)))))
  :config
  ;; Sets the buffer name of org source blocks properly
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name)))
      ad-do-it
      (setq buffer-file-name file-name)))
  (setq org-agenda-files (list org-agenda-dir))
  (setq org-capture-templates
        '(("t" "TODO" entry (file (lambda () (expand-file-name "todo.org" org-agenda-dir)))
           "* TODO %? %^G \n  %U" :empty-lines 1)
          ("d" "Deadline" entry (file (lambda () (expand-file-name "todo.org" org-agenda-dir)))
           "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
          ("p" "Priority" entry (file (lambda () (expand-file-name "todo.org" org-agenda-dir)))
           "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
          ("a" "Appointment" entry (file+headline
                                    (lambda () (expand-file-name "calendar.org" org-agenda-dir))
                                    "Event")
           "* %? %^G \n  %^t")
          ("l" "Link" entry (file+headline
                             (lambda () (expand-file-name "notes.org" org-agenda-dir))
                             "Links")
           "* TODO %a %? %^G\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
          ("n" "Note" entry (file+headline
                             (lambda () (expand-file-name "notes.org" org-agenda-dir))
                             "Notes")
           "* %? %^G\n%U" :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree
                                (lambda () (expand-file-name "journal.org" org-agenda-dir))
                                "Journal")
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
  :requires (markdown-mode org)
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode))
  :bind (:map markdown-mode-map
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point)))

(use-package org-ref
  :requires org
  :defines (papers-dir papers-pdfs papers-notes papers-refs)
  :init
  (defvar papers-dir (expand-file-name "papers" org-remote-dir))
  (defvar papers-pdfs (expand-file-name "lib" papers-dir))
  (defvar papers-notes (expand-file-name "notes.org" papers-dir))
  (defvar papers-refs (expand-file-name "index.bib" papers-dir))
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes papers-notes
        org-ref-default-bibliography (list papers-refs)
        org-ref-pdf-directory papers-pdfs))

(use-package org-noter
  :requires (org org-ref)
  :commands org-noter
  :config
  (setq org-noter-default-notes-file-names '("index-org")
        org-noter-notes-search-path (list papers-dir)
        org-noter-auto-save-last-location t
        org-noter-doc-split-fraction '(0.8 . 0.8)
        org-noter-always-create-frame nil
        org-noter-insert-note-no-questions t
        org-noter-notes-window-location 'vertical-split))

(use-package org-bullets
  :requires org
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("●" "○" "✸" "✿")))

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package ob-async)

(use-package interleave
  :requires org
  :bind (:map global-map
              ("C-x i" . interleave-mode))
  :config
  (setq interleave-split-direction 'horizontal
        interleave-split-lines 20
        interleave-disable-narrowing t))

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
  :requires (flyspell hydra)
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
  :requires markdown-mode)

;;; * Programming Languages
;;;; * C/C++
(use-package irony
  :hook ((c-mode . irony-mode)
         (c-mode . flyspell-prog-mode)
         (objc-mode . irony-mode)
         (objc-mode . flyspell-prog-mode)
         (c++-mode .irony-mode)
         (c++-mode . flyspell-prog-mode))
  :config
  (setq-local company-backends (append '(company-clang) company-backends)))

(use-package flycheck-irony
  :requires (flycheck irony))

;;;; * Elm
(use-package elm-mode
  :requires company
  :hook (elm-mode . flyspell-prog-mode)
  :config
  (setq elm-format-on-save t)
  (setq-local company-backends (append '(company-elm) company-backends)))

;;;; * Haskell
(use-package haskell-mode
  :hook ((haskell-mode . lsp)
         (haskell-mode . flyspell-prog-mode)))

(use-package flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

;;;; * Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;;;; * Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . lsp)
         (python-mode . flyspell-prog-mode))
  :config (setq python-indent-offset 4))

;;;; * Rust
(use-package rust-mode
  :hook ((rust-mode . lsp)
         (rust-mode . flyspell-prog-mode)))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;;;; * Fish
(use-package fish-mode
  :mode "\\.fish\\'")

;;;; * Typescript
(use-package tide
  :requires (flycheck company)
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
  :mode ("\\.ts\\'")
  :hook (typescript-mode . setup-tide-mode))

;;;; * Javascript
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config (setq js2-basic-offset 4))

;;;; * HTML/JSX/RSX
(use-package web-mode
  :requires (flycheck tide)
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
  :hook (web-mode . (lambda ()
                      (when (and
                             buffer-file-name
                             (equal "tsx" (file-name-extension buffer-file-name)))
                        (setup-tide-mode))))
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2

        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-quoting nil
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
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (pdf-misc-size-indication-minor-mode)
                           (pdf-links-minor-mode)
                           (pdf-isearch-minor-mode)
                           (cua-mode 0)))
  :bind
  (:map pdf-view-mode-map
        ("/" . hydra-pdftools/body)
        ("C-s" . isearch-forward)
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

;;; * LaTeX
(use-package tex-site
  :requires (tex latex)
  :ensure auctex
  :hook ((LaTeX-mode . turn-off-auto-fill)
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
  :hook (latex-mode . company-auctex-init))

(use-package company-bibtex
  :requires (company bibtex)
  :hook
  (tex-mode . company-bibtex-setup)
  (latex-mode . company-bibtex-setup)
  (org-mode . company-bibtex-setup)
  :init
  (defun company-bibtex-setup ()
    (setq-local company-backends
                (append
                 '(company-bibtex)
                 company-backends))))

(use-package company-reftex
  :requires (company reftex)
  :hook
  (tex-mode . company-reftex-setup)
  (latex-mode . company-reftex-setup)
  (org-mode . company-reftex-setup)
  :init
  (defun company-reftex-setup ()
    (setq-local company-backends
                (append
                 '(company-reftex-labels company-reftex-citations)
                 company-backends))))

(use-package company-math
  :requires company
  :hook
  (tex-mode . company-math-setup)
  (latex-mode . company-math-setup)
  (org-mode . company-math-setup)
  :init
  (defun company-math-setup ()
    (setq-local company-backends
                (append
                 '(company-math-symbols-latex company-latex-commands)
                 company-backends))))

(use-package auctex-latexmk
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package reftex
  :ensure auctex
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

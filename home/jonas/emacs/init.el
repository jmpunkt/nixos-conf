;;; init.el --- Initilizes Emacs
;;; Commentary:

;;; Code:

(require 'seq)
(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)
(eval-when-compile (require 'use-package))

;;; Startup Improvements
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold 100000000
                  read-process-output-max (* 1024 1024))))

;;; * Paths

(defconst org-remote-dir (expand-file-name "~/Dropbox"))
(defconst org-agenda-dir (expand-file-name "agenda" org-remote-dir))
(defconst org-papers-dir (expand-file-name "papers" org-remote-dir))
(defconst org-papers-pdfs (expand-file-name "lib" org-papers-dir))
(defconst org-papers-notes (expand-file-name "notes.org" org-papers-dir))
(defconst org-papers-bibtex (expand-file-name "index.bib" org-papers-dir))

;;; * Theme
(use-package all-the-icons
  :demand t
  :init (setq inhibit-compacting-font-caches t))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t
        doom-modeline-lsp t))

;;; * Emacs
(use-package emacs
  :demand t
  :hook ((prog-mode . (lambda ()
                        (auto-fill-mode 1)
                        (setq-local comment-auto-fill-only-comments t))))
  :init
  (setq-default tab-width 4
                indent-tabs-mode nil
                show-paren-delay 0
                show-trailing-whitespace t)


  (setq indent-line-function 'insert-tab
        revert-without-query '(".+\.pdf" ".+\.png" ".+\.jpg")
        make-backup-files nil
        auto-save-default nil
        column-number-mode t
        frame-title-format "%b")

  (set-fontset-font "fontset-default"
                    'symbol (font-spec :family "Symbola"))

  (save-place-mode 1)
  (show-paren-mode 1)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode 1)
  (global-prettify-symbols-mode t)
  (global-undo-tree-mode 1)

  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-eldoc-mode -1)

  (custom-set-variables
   '(custom-safe-themes
     (quote
      ("79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default))))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight normal :height 113 :width normal)))))

  (load-theme 'doom-vibrant))

;;; * Core Packages
;;;; * Evil
(use-package evil
  :demand t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

;;;; * DirEnv
(use-package direnv
  :demand t
  :config
  (direnv-mode)
  ;; ensures that direnv is loaded and allowing lsp to find executable
  (advice-add 'prog-mode :before #'direnv-update-environment))

;;;; * Which-Key
(use-package which-key
  :demand t
  :config
  (which-key-mode))

;;;; * Outshine
(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode))

;;;; * Hydra
(use-package hydra
  :demand t
  :bind (:map global-map
              ("C-x w" . my-hydra-window/body))
  :init
  (defhydra my-hydra-window ()
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
                     'my-hydra-window/body)))
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
                     'my-hydra-window/body)))
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
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3))

;;;; * Spelling
(use-package flyspell
  :hook ((prog-mode . enable-flyspell)
         (text-mode . enable-flyspell))
  :bind (:map flyspell-mode-map
              ("C-c s" . my-hydra-spelling/body))
  :init
  (defun enable-flyspell ()
      (when
          (not (memq major-mode
                     '(dired-mode
                       log-edit-mode
                       compilation-mode
                       help-mode
                       profiler-report-mode
                       speedbar-mode
                       gud-mode
                       calc-mode
                       Info-mode)))
        (flyspell-mode 1)))
  (defhydra my-hydra-spelling (:color blue)
    "^
  ^Spelling^          ^Errors^            ^Checker^
------------------------------------------------------------
  [_q_] quit          [_<_] previous      [_c_] correction
  ^^                  [_>_] next          [_d_] dictionary
  ^^                  [_f_] check         ^^
  ^^                  ^^                  ^^"
    ("q" nil)
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" flyspell-correct-at-point)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer))
  :config
  (setq-default ispell-program-name "/run/current-system/sw/bin/hunspell"
                ispell-really-hunspell t
                ; Hide all default entries which may not be available
                ; on the system anyways
                ispell-dictionary-base-alist nil
                ispell-local-dictionary "en_US"))

(use-package flyspell-correct
  :after flyspell)

(use-package flyspell-correct-ivy
  :after (flyspell-correct ivy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package langtool
  :bind (:map global-map
              ("C-c l" . my-hydra-langtool/body))
  :init
  (defhydra my-hydra-langtool (:color blue)
    "^
  ^Spelling^          ^Errors^            ^Checker^
------------------------------------------------------------
  [_q_] quit          [_<_] previous      [_c_] correction
  [_f_] check         [_>_] next          [_s_] show error
  [_d_] done          ^^                  ^^
  ^^                  ^^                  ^^"
    ("q" nil)
    ("<" langtool-goto-previous-error :color pink)
    (">" langtool-goto-next-error :color pink)
    ("c" langtool-correct-buffer)
    ("d" langtool-check-done)
    ("s" langtool-show-message-at-point)
    ("f" langtool-check))
  :config
  ;; for NixOS use languagetool-commandline?
  (setq langtool-bin "languagetool-commandline"))

;;;; * Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
        dashboard-startup-banner nil
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-footer nil
        dashboard-items '((projects . 5)
                          (agenda . 5))))

;;;; * Smartparens
(use-package smartparens
  :hook (eval-expression-minibuffer-setup . smartparens-mode)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem))

;;;; * Projectile
(use-package projectile
  :demand t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action
        projectile-enable-caching t
        projectile-create-missing-test-files t
        projectile-switch-project-action #'projectile-commander
        projectile-ignored-project-function 'file-remote-p
        projectile-completion-system 'ivy)
  (projectile-mode))

(use-package org-projectile
  :bind (:map global-map
              ("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (setq org-projectile-projects-file (expand-file-name "projectile.org" org-remote-dir)
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

;;;; * Treemacs
(use-package treemacs
  :bind (:map global-map
              ([f8] . treemacs)
              ("C-c f" . treemacs-select-window))
  :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :init
  (setq-default treemacs-python-executable "python3")
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
  :after (treemacs projectile))

(use-package treemacs-evil
  :after (treemacs evil))

;;;; * Language Server (LSP)
(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map lsp-mode-map
              ([f6] . my-hydra-lsp/body))
  :init
  (defhydra my-hydra-lsp (:exit t :hint nil)
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
        lsp-document-sync-method nil
        lsp-eldoc-render-all nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-enable-xref t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-snippet t
        lsp-enable-folding nil
        lsp-completion-provider :capf
        lsp-file-watch-ignored '(
                                 "[/\\\\]\\.direnv$"
                                 "[/\\\\]\\.git$"
                                 "[/\\\\]\\.hg$"
                                 "[/\\\\]\\.bzr$"
                                 "[/\\\\]_darcs$"
                                 "[/\\\\]\\.svn$"
                                 "[/\\\\]_FOSSIL_$"
                                 "[/\\\\]\\.idea$"
                                 "[/\\\\]\\.ensime_cache$"
                                 "[/\\\\]\\.eunit$"
                                 "[/\\\\]node_modules$"
                                 "[/\\\\]\\.fslckout$"
                                 "[/\\\\]\\.tox$"
                                 "[/\\\\]\\.stack-work$"
                                 "[/\\\\]\\.bloop$"
                                 "[/\\\\]\\.metals$"
                                 "[/\\\\]target$"
                                 "[/\\\\]\\.deps$"
                                 "[/\\\\]build-aux$"
                                 "[/\\\\]autom4te.cache$"
                                 "[/\\\\]\\.reference$")))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable nil))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;;;; * Completion
;;;;; * Ivy
(use-package ivy
  :bind (:map global-map
              ("C-s" . swiper)
              ("C-x b" . ivy-switch-buffer)
              ("C-x C-b" . ivy-switch-buffer-other-window))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package counsel
  :bind (:map global-map
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :bind (:map projectile-mode-map
              ("C-c p" . my-hydra-projectile/body))
  :init
  (defhydra my-hydra-projectile (:exit t :hint nil)
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

;;;;; * Snippets
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;;;;; * Company
(use-package company
  :bind (:map company-active-map
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next))
  :init (global-company-mode t)
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-require-match nil
        company-backends '(company-files
                           company-capf
                           company-yasnippet
                           company-abbrev
                           company-dabbrev)))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

;;;; * Git
(use-package magit
  :bind (:map global-map
              ("C-x g" . magit-status))
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
          magit-insert-unpushed-to-pushremote)))

(use-package evil-magit
  :after magit)

(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1)
  (add-to-list 'hl-todo-keyword-faces '("TODO" . "gold1"))
  (add-to-list 'hl-todo-keyword-faces '("TEST" . "SpringGreen1"))
  (add-to-list 'hl-todo-keyword-faces '("NOTICE" . "chartreuse3"))
  (add-to-list 'hl-todo-keyword-faces '("FIXME" . "gold1"))
  (add-to-list 'hl-todo-keyword-faces '("HACK" . "DarkOrange1")))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 100))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;;; * Org
(use-package org
  :bind (:map global-map
              ("C-c c" . org-capture))
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . (lambda ()
                       (setq-local tab-width 2)
                       (auto-fill-mode 1)))
         (org-mode . org-indent-mode)
         (org-src-mode . (lambda ()
                           (setq-local
                            flycheck-disabled-checkers
                            '(emacs-lisp-checkdoc))))
         (org-babel-after-execute . (lambda ()
                                      (when org-inline-image-overlays
                                        (org-redisplay-inline-images)))))
  :config
  (add-to-list 'ispell-skip-region-alist
               '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist
               '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist
               '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Sets the buffer name of org source blocks properly
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name)))
      ad-do-it
      (setq buffer-file-name file-name)))
  (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "…"
        org-log-done 'time
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-catch-invisible-edits 'smart
        org-deadline-warning-days 14
        org-agenda-files (list org-agenda-dir)
        ;; `!` ensures that timestamps are used
        org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
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
           "* %? %^G \n  %^t")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (js . t)
     (python . t)
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (plantuml . t)
     (mermaid . t)
     (gnuplot . t)
     (sql . t)
     (dot . t))))

(use-package ox-latex
  :config
  (setq org-latex-listings 'minted
        org-latex-prefer-user-labels t
        org-latex-packages-alist '(("" "minted") ("" "xcolor") ("" "listings") ("" "url"))
        org-latex-compiler "xelatex"
        org-latex-pdf-process '("latexmk -g -pdf -pdflatex=\"%latex -synctex=1 -shell-escape -interaction=nonstopmode\" -outdir=%o %f"))
  (add-to-list
   'org-latex-classes
   '("tufte-book"
     "\\documentclass{tufte-book}
\\ifxetex
  \\newcommand{\\textls}[2][5]{%
    \\begingroup\\addfontfeatures{LetterSpace=#1}#2\\endgroup
  }
  \\renewcommand{\\allcapsspacing}[1]{\\textls[15]{#1}}
  \\renewcommand{\\smallcapsspacing}[1]{\\textls[10]{#1}}
  \\renewcommand{\\allcaps}[1]{\\textls[15]{\\MakeTextUppercase{#1}}}
  \\renewcommand{\\smallcaps}[1]{\\smallcapsspacing{\\scshape\\MakeTextLowercase{#1}}}
  \\renewcommand{\\textsc}[1]{\\smallcapsspacing{\\textsmallcaps{#1}}}
\\fi"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("koma-article"
     "\\documentclass{scrartcl}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("spec"
     "\\documentclass{refart}
      \\pagestyle{plain}
      \\usepackage{makeidx}
      \\usepackage{ifthen}
      \\usepackage{bookmark}
      \\bibliographystyle{alpha}
      \\pagestyle{headings}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("tud-report"
     "\\documentclass{tudapub}
      \\usepackage[AUTO]{babel}
      \\usepackage[botto1]{footmisc}
      \\usepackage{amsbsy,amscd,amsfonts,amstext,amsmath,latexsym,theorem}
      \\pagestyle{plain}
      \\bibliographystyle{alpha}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("tud-exercise"
     "\\documentclass{tudaexercise}
      \\usepackage[AUTO]{babel}
      \\usepackage[botto1]{footmisc}
      \\usepackage{amsbsy,amscd,amsfonts,amstext,amsmath,latexsym,theorem}
      \\pagestyle{plain}
      \\bibliographystyle{alpha}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ox-extra
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package toc-org
  :commands toc-org-mode
  :hook ((org-mode . toc-org-mode)))

(use-package org-ref
  :after org
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes org-papers-notes
        org-ref-default-bibliography (list org-papers-bibtex)
        org-ref-pdf-directory (list org-papers-pdfs)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("●" "○" "✸" "✿")))

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package ob-async
  :after org)

;;; * Configuration Files

;;;; * Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

;;;; * Mermaid
(use-package mermaid-mode)

;;;; * Bazel
(use-package bazel-mode
  :mode ("BUILD\\'" "\\.bzl\\'")
  :bind (:map bazel-mode-map
              ("C-c C-f" . bazel-mode-buildifier)))

;;;; * JSON
(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

;;;; * YAML
(use-package yaml-mode
  :hook (yaml-mode . lsp)
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
  :bind (:map markdown-mode-map
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point))
  :hook (markdown-mode . toc-org-mode)
  :mode
  ("INSTALL\\'"
   "CONTRIBUTORS\\'"
   "LICENSE\\'"
   "README\\'"
   "\\.markdown\\'"
   "\\.md\\'"))

;;;; * Graphivz
(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

;;; * Programming Languages

;;;; * SQL
(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :config
  (setq-default sql-database "development"
                sql-server "localhost")
  (setq-local tab-width 2)
  (sql-highlight-postgres-keywords))

;;;; * C/C++
(use-package irony
  :hook ((c-mode . irony-mode)
         (objc-mode . irony-mode)
         (c++-mode . irony-mode)
         (irony-mode . lsp))
  :config
  (setq-local company-backends (append '(company-clang) company-backends)))

(use-package flycheck-irony
  :after (flycheck irony))

;;;; * Haskell
(use-package haskell-mode
  :hook (haskell-mode . lsp))

(use-package flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

;;;; * Nix
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp)
  :bind (:map nix-mode-map
              ("C-c C-f" . nix-format-buffer))
  :config
  (setq-default nix-nixfmt-bin "nixpkgs-fmt"))

;;;; * Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp)
  :config (setq python-indent-offset 4))

;;;; * Rust
(use-package rust-mode
  :hook (rust-mode . lsp)
  :config
  (setq-default lsp-rust-server 'rust-analyzer
                lsp-rust-analyzer-server-command "rust-analyzer"))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

;;;; * Fish
(use-package fish-mode
  :mode "\\.fish\\'")

;;;; * Java
(use-package lsp-java
  :hook (java-mode . lsp))

;;;; * Web Development (TS[X], JS[X], HTML)
(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :hook (web-mode . lsp)
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-pairing nil
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t)
  :config
  ;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
  (defun web-mode-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face))
           rlt)
      (cond
       ;; Check the words with these font faces, possibly.
       ;; this *blacklist* will be tweaked in next condition
       ((not (memq f '(web-mode-html-attr-value-face
                       web-mode-html-tag-face
                       web-mode-html-attr-name-face
                       web-mode-constant-face
                       web-mode-doctype-face
                       web-mode-keyword-face
                       web-mode-comment-face ;; focus on get html label right
                       web-mode-function-name-face
                       web-mode-variable-name-face
                       web-mode-css-property-name-face
                       web-mode-css-selector-face
                       web-mode-css-color-face
                       web-mode-type-face
                       web-mode-block-control-face)))
        (setq rlt t))
       ;; check attribute value under certain conditions
       ((memq f '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                  (thing-at-point 'symbol)))))
       ;; finalize the blacklist
       (t
        (setq rlt nil)))
      rlt))
  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
        web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

;;; * LaTeX
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . turn-off-auto-fill)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . lsp)
         (LaTeX-mode . outline-minor-mode))
  :config
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
  (add-to-list 'LaTeX-verbatim-environments "comment"))

(use-package nixos-config
  :demand t)

;;; * -- End
(provide 'init)
;;; init.el ends here

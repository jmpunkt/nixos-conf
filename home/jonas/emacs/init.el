;;; init.el --- Initilizes Emacs
;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))

;;; Startup Improvements
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold 100000000
                  read-process-output-max (* 1024 1024))))

(require 'bind-key)
(setq use-package-verbose t)
(setq garbage-collection-messages t)

;;; * Paths
(use-package nixos-paths
  :demand t)

(defconst org-remote-dir (expand-file-name "~/Dropbox"))
(defconst org-agenda-dir (expand-file-name "agenda" org-remote-dir))
(defconst org-papers-dir (expand-file-name "papers" org-remote-dir))
(defconst org-papers-pdfs (expand-file-name "lib" org-papers-dir))
(defconst org-papers-notes (expand-file-name "notes.org" org-papers-dir))
(defconst org-papers-bibtex (expand-file-name "index.bib" org-papers-dir))

;;; * Theme
(use-package all-the-icons
  :demand t
  :init
  (setq inhibit-compacting-font-caches t))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-theme "doom-colors"
        doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  (custom-set-variables
   '(custom-safe-themes
     (quote
      ("79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default))))
  (load-theme 'doom-vibrant))

(use-package doom-modeline
  :demand t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-lsp t
        doom-modeline-height 30
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil))

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
        delete-by-moving-to-trash t
        frame-title-format "%b"
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        backup-directory-alist `((".*" . ,temporary-file-directory)))
  (save-place-mode 1)
  (show-paren-mode 1)
  (global-display-line-numbers-mode t)
  (global-hl-line-mode 1)
  (global-undo-tree-mode 1)

  (global-prettify-symbols-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-eldoc-mode -1)

  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 112
                      :weight 'light)
  (set-face-attribute 'variable-pitch nil
                      :family "JetBrains Mono"
                      :height 1.0)
  (set-face-attribute 'fixed-pitch nil
                      :family "JetBrains Mono"
                      :height 1.0))

(use-package ligature
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("==" "===" "!=" "!==" "=!=" "=:=" "=/="
                            "<=" ">=" "<>" "-|" "_|_" "|-" "||-" "|="
                            "||=" "^=" "<+>" "<+" "+>" "<*>" "<*" "*>"
                            "<!--" "<#--" "-->" "->" "->>" "<<-" "<-"
                            "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>"
                            "=>" "=>>" ">=>" ">>=" ">>-" ">-" ">--"
                            "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>"
                            "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>"
                            "~>" "[||]" "|]" "[|" "|}" "{|" "[<" ">]"
                            "|>" "<|" "||>" "<||" "|||>" "<|||" "<|>"
                            ":=" "::=" "/=" "//=" "/==" ))

  (global-ligature-mode t))

;;; * Core Packages
;;;; * Evil
(use-package evil
  :demand t
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  ; use Xref to find references
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-motion-state-map "gd" nil))

;;;; * DirEnv
(use-package direnv
  :demand t
  :config
  (direnv-mode 1)
  ;; ensures that direnv is loaded and allowing lsp to find executable
  (advice-add 'prog-mode :before #'direnv-update-environment))

;;;; * Which-Key
(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

;;;; * Flycheck
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3))

;;;; * Spelling
(use-package flyspell
  :hook ((prog-mode . (lambda ()
                        (when (not (memq major-mode flyspell-disabled-modes))
                          (flyspell-prog-mode))))
         (text-mode . (lambda ()
                        (when (not (memq major-mode flyspell-disabled-modes))
                          (flyspell-mode 1)))))
  :bind (:map flyspell-mode-map
              ("C-c s <" . flyspell-correct-previous )
              ("C-c s >" . flyspell-correct-next)
              ("C-c s c" . flyspell-correct-at-point)
              ("C-c s d" . ispell-change-dictionary)
              ("C-c s f" . flyspell-buffer))
  :init
  (defvar flyspell-disabled-modes
    '(dired-mode
      log-edit-mode
      compilation-mode
      help-mode
      profiler-report-mode
      speedbar-mode
      gud-mode
      calc-mode
      Info-mode))
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
              ("C-c l <" . langtool-goto-previous-error)
              ("C-c l >" . langtool-goto-next-error)
              ("C-c l c" . langtool-correct-buffer)
              ("C-c l q" . langtool-check-done)
              ("C-c l m" . langtool-show-message-at-point)
              ("C-c l f" . langtool-check))
  :config
  ;; for NixOS use languagetool-commandline?
  (setq langtool-bin "languagetool-commandline"))

;;;; * Dashboard
(use-package dashboard
  :config
  (setq dashboard-center-content t
        dashboard-startup-banner t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-footer nil
        dashboard-items nil)
  (dashboard-setup-startup-hook))

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
  (setq projectile-completion-system 'ivy)

  (projectile-register-project-type 'nix-flake '("flake.nix")
                                    :project-file "flake.nix"
                                    :test "nix flake check")
  (projectile-mode 1))

;;;; * Treemacs
(use-package treemacs
  :demand t
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
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-is-never-other-window nil
        treemacs-eldoc-display nil)
  (treemacs-follow-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

;;;; * Language Server (LSP)
(use-package lsp-mode
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map lsp-mode-map
              ("C-c k d" . lsp-find-declaration)
              ("C-c k D" . lsp-ui-peek-find-definitions)
              ("C-c k R" . lsp-ui-peek-find-references)
              ("C-c k i" . lsp-ui-peek-find-implementation)
              ("C-c k t" . lsp-find-type-definition)
              ("C-c k s" . lsp-signature-help)
              ("C-c k o" . lsp-describe-thing-at-point)
              ("C-c k r" . lsp-rename)
              ("C-c C-f" . lsp-format-buffer)
              ("C-c k m" . lsp-ui-imenu)
              ("C-c k x" . lsp-execute-code-action)
              ("C-c k S" . lsp-workspace-shutdown))
  :config
  (setq lsp-print-performance nil
        lsp-log-io nil
        lsp-eldoc-render-all nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-relative-indentation nil
        lsp-headerline-breadcrumb-enable nil

        lsp-completion-enable t
        lsp-enable-semantic-highlighting t
        lsp-enable-xref t
        lsp-enable-indentation t
        lsp-enable-snippet t))

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
  :demand t
  :bind (:map global-map
              ("C-x b" . ivy-switch-buffer)
              ("C-c C-r" . ivy-resume)
              ("C-x C-b" . ivy-switch-buffer-other-window))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil
        ivy-count-format "(%d/%d)"
        ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package counsel
  :after ivy
  :bind (:map global-map
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file)
              ("C-c c" . counsel-org-capture)
              ("C-c C-g" . counsel-search)))


(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode t))

(use-package ivy-xref
  :after xref
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package swiper
  :after ivy
  :bind (:map global-map
              ("C-s" . swiper)))

(use-package counsel-projectile
  :bind (:map projectile-mode-map
              ("C-c p b" . counsel-projectile-switch-to-buffer)
              ("C-c p d" . counsel-projectile-find-dir)
              ("C-c p D" . projectile-dired)
              ("C-c p f" . counsel-projectile-find-file)
              ("C-c p i" . projectile-invalidate-cache)
              ("C-c p k" . projectile-kill-buffers)
              ("C-c p p" . counsel-projectile-switch-project)
              ("C-c p r" . projectile-replace)
              ("C-c p R" . projectile-replace-regexp)
              ("C-c p s" . counsel-projectile-rg)
              ("C-c p S" . projectile-save-project-buffers)))

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

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t))

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
          magit-insert-unpushed-to-pushremote))
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

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
  (setq magit-todos-depth 100))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;;; * Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . (lambda ()
                       (setq-local tab-width 2)
                       (auto-fill-mode 1)
                       (add-to-list 'ispell-skip-region-alist
                                    '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
                       (add-to-list 'ispell-skip-region-alist
                                    '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
                       (add-to-list 'ispell-skip-region-alist
                                    '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))))
         (org-babel-after-execute . (lambda ()
                                      (when org-inline-image-overlays
                                        (org-redisplay-inline-images)))))
  :config

  (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "…"
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-deadline-warning-days 14
        org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

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

(use-package org-agenda
  :after org
  :config
  (setq org-agenda-files (list org-agenda-dir)))

(use-package org-capture
  :after org
  :config
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
           "* %? %^G \n  %^t"))))

(use-package org-indent
  :after org
  :hook (org-mode . org-indent-mode))

(use-package org-src
  :after org
  :hook (org-src-mode . (lambda ()
                          (setq-local
                           flycheck-disabled-checkers
                           '(emacs-lisp-checkdoc))))
  :config
  ;; Sets the buffer name of org source blocks properly
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name)))
      ad-do-it
      (setq buffer-file-name file-name)))
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t))

(use-package ox-latex
  :defer t
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
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package toc-org
  :hook ((org-mode . toc-org-mode)))

(use-package org-ref
  :commands org-ref-ivy-cite
  :init
  ; prevent Helm from loading
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-bibliography-notes org-papers-notes
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
  :bind (:map json-mode-map
              ("C-c C-f" . json-pretty-print-buffer))
  :config
  (setq js-indent-level 2))

;;;; * YAML
(use-package yaml-mode
  :hook (yaml-mode . lsp)
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;;;; * Meson
(use-package meson-mode
  :mode ("meson.build\\'")
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

;;;; * Haskell
(use-package haskell-mode
  :hook (haskell-mode . lsp))

(use-package flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

;;;; * Scala

(use-package scala-mode
  :hook  (scala-mode . lsp)
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :after (lsp scala-mode)
  :config (setq lsp-metals-treeview-show-when-views-received t))

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
(use-package rustic
  :bind (:map rustic-mode-map
               ("C-c k t" . rustic-cargo-test)
               ("C-c k c" . rustic-cargo-clippy)
               ("C-c k o" . rustic-cargo-outdated)
               ("C-c k b" . rustic-compile)
               ("C-c C-f" . rustic-format-buffer))
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-rustfmt-config-alist '((edition . "2018"))))

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
  :hook ((web-mode . lsp)
         (web-mode . prettier-js-mode))
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
  (with-eval-after-load 'lsp-mode
    (when (derived-mode-p 'web-mode)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map lsp-mode-map)
        (define-key map (kbd "C-c C-f") 'prettier-js)
        (push `(lsp-mode . ,map)
              minor-mode-overriding-map-alist))))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package prettier-js)

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

;;; * -- End
(provide 'init)
;;; init.el ends here

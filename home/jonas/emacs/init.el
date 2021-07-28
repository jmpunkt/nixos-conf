;;; init.el --- Initilizes Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
(eval-when-compile (require 'use-package))
(require 'bind-key)

;;; * Profiler
(use-package esup
  :commands (esup)
  :init
  (setq esup-depth 0))

;;; * Paths
(use-package nixos-paths
  :demand t)

(defconst org-remote-dir (expand-file-name "~/Dropbox"))
(defconst org-agenda-dir (expand-file-name "agenda" org-remote-dir))
(defconst org-papers-dir (expand-file-name "papers" org-remote-dir))
(defconst org-papers-pdfs (expand-file-name "lib" org-papers-dir))
(defconst org-papers-notes (expand-file-name "notes.org" org-papers-dir))

;;; * Theme
(use-package all-the-icons
  :init
  (setq inhibit-compacting-font-caches t))

(use-package doom-themes
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
      ("0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" default))))
  (load-theme 'doom-vibrant)

  (set-face-attribute 'default nil
                      :font "JetBrains Mono"
                      :height 110
                      :weight 'light
                      :width 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font "JetBrains Mono"
                      :height 1.0)
  (set-face-attribute 'fixed-pitch nil
                      :font "JetBrains Mono"
                      :height 1.0))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-lsp t
        doom-modeline-height 30
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-to-project))

;;; * Emacs
(use-package emacs
  :demand t
  :hook ((prog-mode . (lambda ()
                        (auto-fill-mode 1)
                        (display-line-numbers-mode 1)
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
        frame-title-format "Emacs"
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        backup-directory-alist `((".*" . ,temporary-file-directory))
        display-line-numbers-grow-only t)
  (save-place-mode 1)
  (show-paren-mode 1)
  (global-hl-line-mode 1)
  (global-undo-tree-mode 1)
  (toggle-scroll-bar -1)

  (global-prettify-symbols-mode -1)
  (global-eldoc-mode -1))

(use-package ligature
  :config
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
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  ;; use Xref to find references
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-motion-state-map "gd" nil))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init `(bookmark
                          calc
                          calendar
                          company
                          compile
                          dashboard
                          debug
                          dictionary
                          diff-mode
                          dired
                          ediff
                          elfeed
                          elisp-mode
                          eshell
                          eww
                          flycheck
                          help
                          info
                          xref
                          man
                          magit
                          magit-todos
                          ,@(when evil-collection-setup-minibuffer '(minibuffer))
                          org-present
                          (pdf pdf-view)
                          (process-menu simple)
                          profiler
                          js2-mode
                          tide
                          typescript-mode
                          which-key
                          python
                          rjsx-mode)))

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
  :bind (:map flycheck-mode-map
              ([f7] . toggle-flycheck-error-buffer))
  :init
  ;; https://github.com/flycheck/flycheck/issues/710#issuecomment-290899713
  (defun toggle-flycheck-error-buffer ()
    "toggle a flycheck error buffer."
    (interactive)
    (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
        (dolist (w (window-list))
          (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
            (delete-window w)
            ))
      (flycheck-list-errors)
      )
    )
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.125))))

;;;; * Spelling
(use-package flyspell
  :hook ((prog-mode . (lambda ()
                        (when (not (memq major-mode flyspell-disabled-modes))
                          (flyspell-prog-mode))))
         (text-mode . (lambda ()
                        (when (not (memq major-mode flyspell-disabled-modes))
                          (flyspell-mode 1)))))
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
                ;; Hide all default entries which may not be available
                ;; on the system anyways
                ispell-dictionary-base-alist nil
                ispell-local-dictionary "en_US"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c s <" . flyspell-correct-previous)
              ("C-c s >" . flyspell-correct-next)
              ("C-c s c" . flyspell-correct-at-point)
              ("C-c s d" . ispell-change-dictionary)
              ("C-c s f" . flyspell-buffer))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-dummy))

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
  :bind (:map projectile-mode-map
              ("C-c p b" . projectile-switch-to-buffer)
              ("C-c p C-b" . projectile-switch-to-buffer-other-window)
              ("C-c p d" . projectile-find-dir)
              ("C-c p D" . projectile-dired)
              ("C-c p f" . affe-find)
              ("C-c p k" . projectile-kill-buffers)
              ("C-c p p" . projectile-switch-project)
              ("C-c p r" . projectile-replace)
              ("C-c p R" . projectile-replace-regexp)
              ("C-c p s" . affe-grep)
              ("C-c p S" . projectile-save-project-buffers))
  :config
  (projectile-register-project-type 'nix-flake '("flake.nix")
                                    :project-file "flake.nix"
                                    :test "nix flake check")
  (projectile-mode 1))

;;;; * Treemacs
(use-package treemacs
  :bind (:map global-map
              ([f8] . treemacs)
              ("C-c f" . treemacs-select-window))
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
        treemacs-eldoc-display nil
        treemacs-read-string-input 'from-minibuffer)
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
              ("M-RET" . lsp-execute-code-action)
              ("C-c k S" . lsp-workspace-shutdown))
  :config
  (setq lsp-print-performance nil
        lsp-log-io nil
        lsp-eldoc-render-all nil
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-relative-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-semantic-tokens-enable nil

        lsp-completion-enable t
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

;;;; RSS
(use-package elfeed
  :commands (elfeed)
  :init
  (setq elfeed-feeds
      '("https://rss.golem.de/rss.php?feed=RSS2.0"
        "https://www.phoronix.com/phoronix-rss.php"
        "https://www.heise.de/security/rss/alert-news-atom.xml"
        "https://www.heise.de/rss/heise-atom.xml"
        "https://www.tagesschau.de/xml/rss2/"
        "https://weekly.nixos.org/feeds/all.rss.xml")))

;;;; WWW
(use-package shr
  :defer t
  :config
  (setq shr-use-colors nil
        shr-bullet "• "
        shr-folding-mode t))

(use-package browse-url
  :defer t
  :config
  (setq browse-url-handlers '((".*youtube.*" . browse-url-firefox)
                              (".*github.*" . browse-url-firefox)
                              ("." . eww-browse-url))))
(use-package eww
  :commands (eww eww-follow-link)
  :hook (eww-mode . (lambda () (setq-local show-trailing-whitespace nil)))
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html?q="))

;;;; Search/Find
(use-package embark
  :bind
  (("C-c r" . embark-act)
   ("C-h B" . embark-bindings)))

(use-package orderless
  :demand t
  :custom (completion-styles '(orderless)))

(use-package selectrum
  :demand t
  :config
  (selectrum-mode +1)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :demand t
  :after (projectile)
  :bind (:map global-map
              ("C-x b" . consult-buffer)
              ("C-x C-b" . consult-buffer-other-window)
              ("M-o" . consult-outline)
              ("C-s" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-.")))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
  (consult-customize affe-grep :preview-key (kbd "M-.")))

;;;;; * Snippets
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :defer 2)

;;;;; * Company
(use-package company
  :init
  (global-company-mode t)
  (company-tng-configure-default)
  :config
  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (advice-add 'company-capf--candidates :around #'just-one-face)
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
  (setq magit-completing-read-function #'selectrum-completing-read)
  (setq magit-diff-refine-hunk 'all)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream))

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

;; (use-package magit-delta
;;   :hook (magit-mode . magit-delta-mode))

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
                       (display-line-numbers-mode 1)
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
  (put 'bibtex-completion-bibliography 'safe-local-variable #'stringp)
  (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "…"
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-deadline-warning-days 14
        org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

(use-package ob-python
  :defer t
  :commands (org-babel-execute:python))

(use-package ob-latex
  :defer t
  :commands (org-babel-execute:latex
             org-babel-expand-body:latex))

(use-package ob-gnuplot
  :defer t
  :commands (org-babel-expand-body:gnuplot))

(use-package ob-dot
  :defer t
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package ob-emacs-lisp
  :defer t
  :commands (org-babel-execute:emacs-lisp
             org-babel-expand-body:emacs-lisp))

(use-package ob-plantuml
  :defer t
  :commands (org-babel-execute:plantuml))

(use-package ob-async
  :commands (ob-async-org-babel-execute-src-block
             org-babel-execute-src-block:async))

(use-package org-agenda
  :defer t
  :config
  (setq org-agenda-files (list org-agenda-dir)))

(use-package org-capture
  :commands (org-capture)
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
  :hook (org-mode . org-indent-mode))

(use-package org-src
  :defer t
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
  :defer t
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package toc-org
  :hook ((org-mode . toc-org-mode)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("●" "○" "✸" "✿")))

(use-package bibtex-actions
  :bind (("C-c b" . bibtex-actions-insert-citation)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset)))

;;; * Configuration Files

;;;; * Dhall
(use-package dhall-mode
  :mode "\\.dhall\\'")

;;;; * Mermaid
(use-package mermaid-mode
  :defer t)

;;;; * Bazel
(use-package bazel
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
  :bind (:map graphql-mode-map
              ("C-c C-f" . prettier-js))
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
  :hook ((markdown-mode . toc-org-mode)
         (markdown-mode . (lambda ()
                            (display-line-numbers-mode 1))))
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
  (setq rustic-lsp-server 'rust-analyzer
        rustic-rustfmt-config-alist '((edition . "2018"))
        rustic-format-trigger 'on-save))

;;;; * Fish
(use-package fish-mode
  :mode "\\.fish\\'")

;;;; * Web Development (TS[X], JS[X], HTML)
(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :hook ((web-mode . lsp)
         (web-mode . prettier-js-mode)
         (web-mode . (lambda ()
                       (with-eval-after-load 'lsp-mode
                         (let ((newmap (copy-keymap lsp-mode-map)))
                           (define-key newmap (kbd "C-c C-f") 'prettier-js)
                           (push `(lsp-mode . ,newmap)
                                 minor-mode-overriding-map-alist))))))
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
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package prettier-js
  :commands prettier-js)

;;; * PDF
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (pdf-misc-size-indication-minor-mode)
                           (pdf-links-minor-mode)
                           (pdf-isearch-minor-mode)
                           (cua-mode -1)))
  :bind
  (:map pdf-view-mode-map
        ("/" . my-hydra-pdftools/body)
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
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1))

;;; * -- End
(provide 'init)
;;; init.el ends here

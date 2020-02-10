;;; init.el --- Initilizes Emacs
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)
(eval-when-compile (require 'use-package))

;;; Startup Improvements

(defvar file-name-handler-alist-backup
  file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold
                  (car (get 'gc-cons-threshold 'standard-value))
                  file-name-handler-alist
                  (append
                   file-name-handler-alist-backup
                   file-name-handler-alist))))

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
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :demand t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t
        doom-modeline-lsp t))

;;; * Emacs
(use-package emacs
  :demand t
  :bind (:map global-map
              ([f1] . eshell))
  :init
  (setq-default tab-width 4
                indent-tabs-mode nil
                show-paren-delay 0
                show-trailing-whitespace t)

  (setq indent-line-function 'insert-tab
        revert-without-query '(".+\.pdf" ".+\.png" ".+\.jpg")
        make-backup-files nil
        auto-save-default nil
        column-number-mode t)

  (set-fontset-font "fontset-default"
                    'symbol (font-spec :family "Symbola"))

  (save-place-mode 1)
  (show-paren-mode 1)
  (global-linum-mode t)
  (global-hl-line-mode 1)
  (global-prettify-symbols-mode t)

  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-eldoc-mode -1)

  (custom-set-variables
   '(custom-safe-themes
     (quote
      ("fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default))))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight normal :height 113 :width normal)))))

  (load-theme 'doom-vibrant))

;;; * Core Packages
;;;; * Evil
(use-package evil
  :demand t
  :config
  (evil-mode 1))

;;;; * DirEnv
(use-package direnv
  :demand t
  :config
  (direnv-mode))

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
  :after direnv
  :init
  (global-flycheck-mode t)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay .3
        flycheck-executable-find
        ;; Uses direnv environment
        (lambda (cmd) (direnv-update-environment default-directory)(executable-find cmd))))

;;;; * Spelling
(use-package flyspell
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c s" . my-hydra-spelling/body))
  :hook (text-mode . flyspell-mode)
  :init
  (defhydra my-hydra-spelling (:color blue)
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
  (setq-default ispell-program-name "hunspell"
        ispell-local-dictionary "en_US"
        ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (defun flyspell-buffer-after-pdict-save (&rest _) (flyspell-buffer))
  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
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

;;;; * Smartparens
(use-package smartparens
  :demand t
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
  :after (org projectile)
  :bind (:map global-map
              ("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (setq org-projectile-projects-file (expand-file-name "projectile.org" org-remote-dir)
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

;;;; * Treemacs
(use-package treemacs
  :demand t
  :bind (:map global-map
              ([f8] . treemacs)
              ("C-c f" . treemacs-select-window))
  :hook (treemacs-mode . (lambda () (linum-mode -1)))
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
  :demand t
  :after (treemacs projectile))

;;;; * Language Server (LSP)
(use-package lsp-mode
  :commands lsp
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
        lsp-enable-xref t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-snippet t
        lsp-prefer-flymake nil))

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package company-lsp
  :commands company-lsp
  :after (company lsp-mode)
  :config
  (setq-local company-backends (append '(company-lsp) company-backends))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

;;;; * Completion
;;;;; * Ivy
(use-package ivy
  :demand t
  :bind (:map global-map
              ("C-s" . swiper)
              ("C-x C-b" . ivy-switch-buffer-other-window))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package ivy-bibtex
  :after (ivy org org-ref bibtex)
  :config
  (setq bibtex-completion-bibliography org-papers-bibtex
        bibtex-completion-library-path org-papers-pdfs
        bibtex-completion-notes-path org-papers-notes))

(use-package counsel
  :demand t
  :bind (:map global-map
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :after (counsel projectile)
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

(use-package yasnippet-snippets)

;;;;; * Company
(use-package company
  :after yasnippet
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
        company-tooltip-limit 20)
  (setq company-backends
        '(company-bbdb
          (company-files
           company-capf
           company-yasnippet)
          (company-dabbrev-code
           company-gtags
           company-etags)
          company-dabbrev)))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))

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
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 100))

;;;; * Eshell
(use-package eshell
  :after ansi-color
  :hook (eshell-mode . (lambda () (linum-mode -1) (setq-local global-hl-line-mode nil)))
  :config
  (defun eshell-handle-ansi-color ()
    (ansi-color-apply-on-region eshell-last-output-start
                                eshell-last-output-end))
  (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

;;;; * Org
(use-package org
  :after (flyspell flycheck)
  :bind (:map global-map
              ("C-c c" . org-capture))
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . flyspell-mode)
         (org-mode . (lambda ()
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
  ;; Sets the buffer name of org source blocks properly
  (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
    (let ((file-name (buffer-file-name)))
      ad-do-it
      (setq buffer-file-name file-name)))
       (setq org-highlight-latex-and-related '(latex)
        org-ellipsis "…"
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
  :after (markdown-mode org)
  :hook ((org-mode . toc-org-mode)
         (markdown-mode . toc-org-mode))
  :bind (:map markdown-mode-map
              ("C-c C-o" . toc-org-markdown-follow-thing-at-point)))

(use-package org-ref
  :after (org hydra)
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes org-papers-notes
        org-ref-default-bibliography (list org-papers-bibtex)
        org-ref-pdf-directory (list org-papers-pdfs)))

(use-package org-noter
  :after (org org-ref)
  :commands org-noter
  :config
  (setq org-noter-default-notes-file-names '("index-org")
        org-noter-notes-search-path (list org-papers-dir)
        org-noter-auto-save-last-location t
        org-noter-doc-split-fraction '(0.8 . 0.8)
        org-noter-always-create-frame nil
        org-noter-insert-note-no-questions t
        org-noter-notes-window-location 'vertical-split))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("●" "○" "✸" "✿")))

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

(use-package ob-async)

(use-package interleave
  :bind (:map global-map
              ("C-x i" . interleave-mode))
  :config
  (setq interleave-split-direction 'horizontal
        interleave-split-lines 20
        interleave-disable-narrowing t))

;;; * Configuration Files

;;;; * Bazel
(use-package bazel-mode
  :mode ("BUILD\\'" "\\.bzl\\'"))

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
  :after flyspell
  :mode
  ("INSTALL\\'"
   "CONTRIBUTORS\\'"
   "LICENSE\\'"
   "README\\'"
   "\\.markdown\\'"
   "\\.md\\'")
  :hook (markdown-mode . flyspell-mode))

(use-package markdown-mode+
  :after markdown-mode)

;;;; * Graphivz

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

;;; * Programming Languages

;; Sets the auto-fill-mode only for comments
(defun my-prog-setup ()
  (auto-fill-mode 1)
  (setq-local comment-auto-fill-only-comments t))

;;;; * SQL

(use-package sql
  :mode ("sql" . sql-mode)
  :hook (sql-mode . (lambda () (setq tab-width 2)))
  :config
  (setq-default sql-database "development"
                sql-server "localhost")
  (sql-highlight-postgres-keywords))

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
  :after (flycheck irony))

;;;; * Elm
(use-package elm-mode
  :after company
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
         (rust-mode . flyspell-prog-mode)
         (rust-mode . my-prog-setup)))

(use-package lsp-rust
  :init
  (setq-default lsp-rust-server 'rust-analyzer))

(use-package flycheck-rust
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
  :mode ("\\.ts\\'")
  :hook (typescript-mode . setup-tide-mode))

;;;; * Java
(use-package lsp-java
  :hook ((java-mode . lsp)
         (java-mode . flyspell-prog-mode)
         (java-mode . my-prog-setup)))

;;;; * Javascript
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config (setq js2-basic-offset 4))

;;;; * HTML/JSX/RSX
(use-package web-mode
  :after (flycheck tide)
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
                           (linum-mode -1)
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
  (setq pdf-view-resize-factor 1.1)
  (defhydra my-hydra-pdftools (:color blue :hint nil)
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
    ("q" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("al" pdf-annot-list-annotations)
    ("ad" pdf-annot-delete)
    ("aa" pdf-annot-attachment-dired)
    ("am" pdf-annot-add-markup-annotation)
    ("at" pdf-annot-add-text-annotation)
    ("y" pdf-view-kill-ring-save)
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
  :after (tex latex)
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
        bibtex-autokey-titleword-length 100
        bibtex-autokey-titlewords 1))

(use-package company-auctex
  :hook (latex-mode . company-auctex-init))

(use-package company-bibtex
  :after (company bibtex)
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
  :after (company reftex)
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
  :after company
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
  (autoload 'LaTeX-preview-setup "preview"))

(use-package nixos-config)

;;; * -- End
(provide 'init)
;;; init.el ends here

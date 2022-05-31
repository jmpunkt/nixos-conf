;;; init.el --- Initilizes Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
(eval-when-compile (require 'use-package))
(require 'bind-key)

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
  (setq doom-modeline-height 30
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-to-project))

;;; * Emacs
(use-package emacs
  :demand t
  :hook ((prog-mode . (lambda ()
                        (electric-indent-local-mode 1)
                        (auto-fill-mode 1)
                        (display-line-numbers-mode 1)
                        (setq-local comment-auto-fill-only-comments t))))
  :init
  (setq-default tab-width 4
                indent-tabs-mode nil
                show-paren-delay 0
                show-trailing-whitespace t)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (setq indent-line-function 'insert-tab
        tab-always-indent 'complete
        revert-without-query '(".+\.pdf" ".+\.png" ".+\.jpg")
        make-backup-files nil
        auto-save-default nil
        column-number-mode t
        delete-by-moving-to-trash t
        frame-title-format "%b"
        icon-title-format "%b"
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        backup-directory-alist `((".*" . ,temporary-file-directory))
        display-line-numbers-grow-only t
        compilation-scroll-output t
        auth-source-save-behavior nil)
  (global-set-key "\t" 'completion-at-point)
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (toggle-scroll-bar -1)
  (global-so-long-mode 1)
  (electric-indent-mode -1)
  (pixel-scroll-precision-mode)

  (global-prettify-symbols-mode -1)
  (global-eldoc-mode -1))

(use-package ligature
  :hook (prog-mode . ligature-mode)
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
                            ":=" "::=" "/=" "//=" "/==" )))

;;;; Shell
(use-package xterm-color)

(use-package esh-mode
  :after xterm-color
  :init
  (defun jmpunkt/evil-collection-eshell-goto-end-or-here ()
    "Smart eshell goto promt for evil.

If the cursor is in the history of eshell, then we want to go to the end of buffer.
If the cursor is on the promt of the eshell, then we want to go to the first writable position.
If the cursor is on the last promt, then we want to insert at the current position."
    (if (< (point) eshell-last-output-start)
        (end-of-buffer)
      (let ((pos (point)))
        (progn
          (while (get-text-property pos 'read-only)
            (setq pos (+ pos 1)))
          (goto-char pos)))))
  (defun jmpunkt/evil-collection-eshell-goto-end-on-insert ()
    "Go to next prompt on `evil' replace/insert enter."
    (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
      (add-hook hook 'jmpunkt/evil-collection-eshell-goto-end-or-here nil t)))
  :hook
  (eshell-mode . (lambda ()
                   (jmpunkt/evil-collection-eshell-goto-end-on-insert)))
  (eshell-before-prompt . (lambda ()
                            (setq-local xterm-color-preserve-properties t)))
  (eshell-pre-command . (lambda ()
                          (setq-local process-environment (copy-sequence process-environment))
                          (setenv "TERM" "xterm-256color")
                          (setenv "PAGER" "cat")))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t))

(use-package compile
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun jmpunkt/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'jmpunkt/advice-compilation-filter))


;;; * Core Packages

;;;; * undo-tree
(use-package undo-tree
  :hook ((prog-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil))

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
  (define-key evil-motion-state-map "gd" nil)
  (define-key evil-motion-state-map "n" nil))

(use-package evil-collection
  :after evil
  :config
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (global-set-key [mouse-1] 'mouse-set-point)
  (global-unset-key [down-mouse-1])
  (global-unset-key [drag-mouse-1])
  (evil-collection-init `(
                          (pdf pdf-view)
                          ,@(when evil-collection-setup-minibuffer '(minibuffer))
                          bookmark
                          calc
                          calendar
                          compile
                          consult
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
                          flymake
                          help
                          help
                          info
                          js2-mode
                          magit
                          magit-todos
                          man
                          org
                          org-present
                          profiler
                          python
                          rjsx-mode
                          term
                          typescript-mode
                          which-key
                          xref)))

(use-package evil-textobj-tree-sitter
  :after tree-sitter
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "n" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
  (define-key evil-inner-text-objects-map "n" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))


;;;; * DirEnv
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;;; * Which-Key
(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

;;;; * xref/jumping
(use-package smart-jump
  :demand t
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references)))

;;;; * Spelling
(use-package ispell
  :config
  (setq-default ispell-program-name "aspell"
                ;; Hide all default entries which may not be available
                ;; on the system anyways
                ispell-extra-args '("--sug-mode=ultra"
                                    "--run-together")
                ispell-dictionary-base-alist nil
                ispell-local-dictionary "en"))

(use-package flyspell
  :init
  (defvar jmpunkt/flyspell-disabled-modes
    '(dired-mode
      log-edit-mode
      compilation-mode
      help-mode
      profiler-report-mode
      speedbar-mode
      gud-mode
      calc-mode
      Info-mode))
  (defun jmpunkt/flyspell-enabled-for-mode ()
    (interactive)
    (not (memq major-mode jmpunkt/flyspell-disabled-modes)))
  (defun jmpunkt/flyspell-generic-progmode-verify ()
    "Taken from `flyspell-generic-progmode-verify`, modified to work
     with item like `(tree-sitter-hl-face:doc tree-sitter-hl-face:comment)`.
     Didnt work previously because of `memq`, now use `member`."
    (unless (eql (point) (point-min))
      (let ((f (get-text-property (1- (point)) 'face)))
        (member f jmpunkt/flyspell-prog-text-faces))))
  (setq jmpunkt/flyspell-prog-text-faces
        '(tree-sitter-hl-face:string
          tree-sitter-hl-face:doc
          tree-sitter-hl-face:comment
          (tree-sitter-hl-face:doc tree-sitter-hl-face:comment)
          font-lock-comment-face
          font-lock-doc-face
          font-lock-string-face))
  :hook ((prog-mode . (lambda ()
                        (when (jmpunkt/flyspell-enabled-for-mode)
                          (setq flyspell-generic-check-word-predicate
                                #'jmpunkt/flyspell-generic-progmode-verify)
                          (flyspell-mode 1)
                          (run-hooks 'flyspell-prog-mode-hook))))
         (text-mode . (lambda ()
                        (when (jmpunkt/flyspell-enabled-for-mode)
                          (flyspell-mode 1)))))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

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

;;;; * Projectile
(use-package project
  :demand t
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("f" . jmpunkt/project-affe-find)
              ("p" . project-switch-project)
              ("r" . project-query-replace-regexp)
              ("g" . magit-project-status)
              ("s" . consult-ripgrep))
  :config
  (setq project-switch-commands
        '((jmpunkt/project-affe-find "file")
          (consult-ripgrep "search")
          (project-find-dir "directory")
          (project-dired "browse")
          (magit-project-status "vc")
          (project-eshell "shell")))
  :init
  (defun jmpunkt/project-affe-find ()
    "Runs `affe-find` in the current project directory."
    (interactive)
    (affe-find (project-root (project-current t))))
  (defun jmpunkt/project-compile-setup (proc)
    "Adds the project root path to the search path for compilation-mode"
    (let ((root (project-root (project-current t))))
      (when root
        (setq-local compilation-search-path `(,root)))))
  (add-hook 'compilation-start-hook #'jmpunkt/project-compile-setup))

;;;; * Language Server (LSP)
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c k r" . eglot-rename)
              ("C-c C-f" . eglot-format-buffer)
              ("M-RET" . eglot-code-actions))
  :init
  (defvar jmpunkt/eglot-keys-map (make-keymap)
    "Keymap which supersedes the default eglot keymap")
  (define-minor-mode jmpunkt/eglot-keys-mode
    "Minor mode allows to overwrite the Eglot keybinds for specific major modes."
    :keymap jmpunkt/eglot-keys-map)
  (add-to-list 'emulation-mode-map-alists
               `((jmpunkt/eglot-keys-mode . ,jmpunkt/eglot-keys-map)))
  (defun jmpunkt/eglot-keys-for (mode-hook)
    "Enables the Eglot key mode for a specific MODE-HOOK"
    (add-hook mode-hook
              (lambda ()
                (add-hook 'eglot-managed-mode-hook
                          (lambda () (jmpunkt/eglot-keys-mode))
                          nil
                          1))))
  :config
  (setq eglot-server-programs
        (append
         (assoc-delete-all 'rust-mode eglot-server-programs)
         '((rust-mode . ("rust-analyzer")))))

  (setq eglot-extend-to-xref t)
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'highlight)

  ;; disable mouse support for flymake face
  (cl-loop for i from 1
           for type in '(eglot-note eglot-warning eglot-error)
           do (put type 'flymake-overlay-control
                   `((priority . ,(+ 50 i))
                     (face . 'flymake-error))))
  (setq eglot-stay-out-of '(company eldoc)
        eglot-confirm-server-initiated-edits nil))

(use-package eglot-x)

;;;; * Flymake
(use-package flymake
  :after consult
  :bind (:map flymake-mode-map
              ([f7] . consult-flymake))
  :hook (prog-mode . flymake-mode))

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
(use-package minibuffer
  :bind (:map minibuffer-local-map
              ("C-j" . next-line)
              ("C-k" . previous-line)
              ("M-j" . scroll-up-command)
              ("M-k" . scroll-down-command)
              ("C-S-j" . forward-paragraph)
              ("C-S-k" . backward-paragraph)
              ("C-b" . beginning-of-buffer)
              ("C-e" . end-of-buffer)
              ("C-n" . next-history-element)
              ("C-m" . previous-history-element)
              ([return] . exit-minibuffer)))

(use-package embark
  :hook (embark-collect-mode . (lambda () (setq show-trailing-whitespace nil)))
  :bind
  (("C-c C-r" . embark-act)
   ("C-h B" . embark-bindings))
  :config
  (defun embark-vertico-indicator ()
    (let ((fr face-remapping-alist))
      (lambda (&optional keymap _targets prefix)
        (when (bound-and-true-p vertico--input)
          (setq-local face-remapping-alist
                      (if keymap
                          (cons '(vertico-current . embark-target) fr)
                        fr))))))

  (add-to-list 'embark-indicators #'embark-vertico-indicator))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-component-separator "[ -/]+"
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :demand t
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :demand t
  :bind (:map global-map
              ("C-x b" . consult-buffer)
              ("C-x C-b" . consult-buffer-other-window)
              ("M-o" . consult-outline)
              ("C-s" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; ++ https://github.com/minad/consult/wiki#narrowing-which-key-help-without-delay
  (defun immediate-which-key-for-narrow (fun &rest args)
    (let* ((refresh t)
           (timer (and consult-narrow-key
                       (memq :narrow args)
                       (run-at-time 0.05 0.05
                                    (lambda ()
                                      (if (eq last-input-event (elt consult-narrow-key 0))
                                          (when refresh
                                            (setq refresh nil)
                                            (which-key--update))
                                        (setq refresh t)))))))
      (unwind-protect
          (apply fun args)
        (when timer
          (cancel-timer timer)))))
  (advice-add #'consult--read :around #'immediate-which-key-for-narrow)
  ;; --
  (setq consult-narrow-key (kbd "C-+"))
  (consult-customize
   affe-grep affe-find
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

(use-package affe
  :after orderless
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler
        affe-find-command "fd -c never -t f")
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-echo-documentation nil)
  (corfu-preselect-first nil)
  (corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ("<escape>". corfu-quit)
              ("<return>" . corfu-insert)
              ("SPC" . corfu-insert-separator)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :hook ((prog-mode . corfu-mode)
         (minibuffer-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :config
  (custom-set-faces '(corfu-current ((t (:inherit vertico-current :background nil :foreground nil)))))
  (set-face-background 'corfu-default (doom-color 'bg))
  (set-face-background 'corfu-bar (doom-color 'base0))
  (set-face-background 'corfu-border (doom-color 'base4))
  (setq corfu-min-width 30)
  (corfu-global-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-quit-at-boundary t
                          corfu-quit-no-match t
                          corfu-auto nil)
              (corfu-mode))))

;;;; * Git
(use-package magit
  :bind (:map global-map
              ("C-x g" . magit-status)
              ("C-x G" . magit-status-here))
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

(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;;;; * Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun jmpunkt/org-delegate-keybind (key)
    "Tries delegating a key sequence to the underlying org-edit-special block.

 Mimics the behavior of entering the block, pressing a single key command, and then exiting."
    (when (org-in-src-block-p)
      (org-edit-special)
      (let ((formatter (key-binding key)))
        (when formatter
          (funcall formatter)))
      (org-edit-src-exit)))
  (defun jmpunkt/format-org-src ()
    "Formats the org-src-block with `jmpunkt/org-delegate-keybind`."
    (interactive)
    (jmpunkt/org-delegate-keybind (kbd "C-c C-f")))
  :bind (:map org-mode-map
              ("C-c C-f" . jmpunkt/format-org-src))
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

(use-package ob-sql
  :defer t
  :commands (org-babel-execute:sql))

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

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset)))

;;; * Configuration Files
(use-package tree-sitter
  :demand t
  :init (setq tsc-dyn-get-from nil)
  :hook ((prog-mode . (lambda () (turn-on-tree-sitter-mode)))
         (text-mode . (lambda () (turn-on-tree-sitter-mode)))))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(latex-mode . latex))
  (add-to-list 'tree-sitter-major-mode-language-alist '(nix-mode . nix))
  (add-to-list 'tree-sitter-major-mode-language-alist '(markdown-mode . markdown))
  (add-to-list 'tree-sitter-major-mode-language-alist '(haskell-mode . haskell))
  (add-to-list 'tree-sitter-major-mode-language-alist '(yaml-mode . yaml))
  (add-to-list 'tree-sitter-major-mode-language-alist '(toml-mode . toml))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

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
  :hook (yaml-mode . eglot-ensure)
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;;;; * TOML
(use-package conf-mode
  :mode (("\\.toml\\'" . toml-mode))
  :init
  (define-derived-mode toml-mode conf-toml-mode "TOML"))

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
  (setq-local tab-width 2)
  (sql-highlight-postgres-keywords))

(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode)
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat-buffer))
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g")))

;;;; * Haskell
(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

;;;; * Nix
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :bind ((:map nix-mode-map
               ("C-c C-f" . nix-format-buffer))
         (:map jmpunkt/eglot-keys-map
               ("C-c C-f" . nix-format-buffer)))
  :init
  (jmpunkt/eglot-keys-for 'nix-mode-hook)
  ;; Added support for alejandra
  (defun jmpunkt/nix--format-call (buf nixfmt-bin)
    "Format BUF using alejandra."
    (let ((stderr (get-buffer-create "*nixfmt-stderr*"))
          (tempfile (make-temp-file "nixfmt")))
      (with-current-buffer stderr
        (erase-buffer))
      (with-current-buffer (get-buffer-create "*nixfmt*")
        (erase-buffer)
        (insert-buffer-substring buf)
        (if (zerop (call-process-region (point-min) (point-max) nixfmt-bin t `(t ,tempfile) nil))
            (progn
              (with-current-buffer stderr (replace-buffer-contents (create-file-buffer tempfile)))
              (nix--replace-buffer-contents (current-buffer) buf))
          (error "Nixfmt failed, see *nixfmt-stderr* buffer for details")
          (with-current-buffer stderr
            (split-window-right))))))
  (advice-add 'nix--format-call :override #'jmpunkt/nix--format-call))

;;;; * Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure)
  :config (setq python-indent-offset 4))

;;;; * Rust
(use-package rust-mode
  :hook (rust-mode . eglot-ensure)
  :init
  (defun jmpunkt/rust-buffer-project ()
    "Get project root if possible."
    (let ((rust-cargo-bin-resolved (executable-find rust-cargo-bin)))
      (message rust-cargo-bin-resolved)
      (when (null rust-cargo-bin-resolved)
        (error "`%s' not found in current path" rust-cargo-bin))
      (with-temp-buffer
        (let ((ret (call-process rust-cargo-bin-resolved nil t nil "locate-project")))
          (when (/= ret 0)
            (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
          (goto-char 0)
          (let ((output (json-read)))
            (cdr (assoc-string "root" output)))))))
  :bind (:map rust-mode-map
              ("C-c k t" . rust-test)
              ("C-c k r" . rust-run)
              ("C-c k c" . rust-check)
              ("C-c k C" . rust-clippy)
              ("C-c k b" . rust-compile)
              ("C-c C-f" . rust-format-buffer))
  :config
  (advice-add 'rust-buffer-project :override #'jmpunkt/rust-buffer-project))

;;;; * Fish
(use-package fish-mode
  :mode "\\.fish\\'")

;;;; * Typescript
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :hook ((typescript-mode . eglot-ensure)
         (typescript-mode . prettier-js-mode))
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2))

;;;; * WEB
(use-package html-mode
  :mode ("\\.html\\'" . html-mode)
  :hook ((html-mode . eglot-ensure)
         (html-mode . prettier-js-mode)))

(use-package css-mode
  :hook ((css-mode . eglot-ensure)
         (scss-mode . prettier-js-mode))
  :bind (:map css-mode-map
              ("C-c C-f" . prettier-js))

  :config
  (setq css-indent-offset 2))

(use-package prettier-js
  :commands prettier-js)

;;;; * LaTeX
(use-package latex-mode
  :mode "\\.tex\\'")

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

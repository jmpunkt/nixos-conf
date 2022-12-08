;;; init.el --- Initilizes Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'nixos-paths)
(require 'treesit)

(use-package esup
  :defer t
  :commands esup
  :config
  (setq esup-depth 0)
  ;; HACK: For NixOS its not possible using the built-in way to
  ;; determine the executable path for Emacs. The built-in method
  ;; finds the Emacs executable/script which does not provide the
  ;; libraries.
  (setq esup-emacs-path (s-trim (shell-command-to-string "command -v emacs"))))

;;; * Paths
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
  (doom-themes-org-config)
  (custom-set-variables
   '(custom-safe-themes
     (quote
      ("c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" default))))
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

;;; * Emacs
(use-package emacs
  :demand t
  :hook ((prog-mode . jmpunkt/prog-init)
         (conf-mode . jmpunkt/conf-init)
         (text-mode . jmpunkt/text-init))
  :bind (:map global-map
              ("TAB" . completion-at-point)
              ("C-+" . text-scale-increase)
              ("C--" . text-scale-decrease)
              ("C-j" . jmpunkt/join-line))
  :init
  (defun jmpunkt/join-line ()
    (interactive)
    (next-line)
    (delete-indentation))
  (defun jmpunkt/default-init ()
    (subword-mode 1)
    (electric-indent-local-mode 1)
    (electric-pair-local-mode 1)
    (auto-fill-mode 1)
    (show-paren-mode 1)
    (display-line-numbers-mode 1)
    (setq-local tab-width 4
                indent-tabs-mode nil
                show-trailing-whitespace t))
  (defun jmpunkt/prog-init ()
    (jmpunkt/default-init)
    (setq-local comment-auto-fill-only-comments t))
  (defun jmpunkt/conf-init ()
    (jmpunkt/default-init)
    (setq-local comment-auto-fill-only-comments t))
  (defun jmpunkt/text-init ()
    (jmpunkt/default-init)
    (setq-local comment-auto-fill-only-comments nil))
  (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
    (let* ((available-width (- (window-width) (length left) 2)))
      (format (format " %%s %%%ds " available-width) left right)))
  (defun jmpunkt/mode-line-file-info ()
    (let ((eol (pcase (coding-system-eol-type buffer-file-coding-system)
                 (0 "LF  ")
                 (1 "CRLF  ")
                 (2 "CR  ")))
          (encoding (coding-system-type buffer-file-coding-system)))
      (format " %s/%s " encoding eol)))
  (defun jmpunkt/mode-line-vc ()
    (when-let* ((backend (vc-backend (buffer-file-name))))
      (format " %s " (replace-regexp-in-string
                      (format "^ %s:" backend)
                      " "
                      vc-mode))))
  (defun jmpunkt/mode-line-buffer-name ()
    (if (and (not buffer-read-only) (buffer-modified-p (current-buffer)))
        (propertize " %b " 'face `(:foreground ,(doom-color 'red) :weight bold))
      (propertize " %b " 'face `(:weight bold))))
  (defun jmpunkt/mode-line-position ()
    (when line-number-mode
      (if column-number-mode
          " %l:%c "
        " %lL ")))
  (defun jmpunkt/mode-line-region ()
    (when (use-region-p)
      (format "%sC %sL"
              (- (use-region-end) (use-region-beginning))
              (count-lines (use-region-beginning) (use-region-end)))))
  (defun jmpunkt/mode-line-flymake ()
    (with-eval-after-load 'flymake
      (when flymake-mode flymake-mode-line-counter-format)))
  (defun jmpunkt/mode-line-major-mode ()
    (format-mode-line '(" " mode-name " ")))
  (setq indent-line-function 'indent-relative
        tab-always-indent 'complete
        revert-without-query '(".+\.pdf" ".+\.png" ".+\.jpg")
        make-backup-files nil
        auto-save-default nil
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
        backup-directory-alist `((".*" . ,temporary-file-directory))
        column-number-mode t
        read-extended-command-predicate #'command-completion-default-include-p
        delete-by-moving-to-trash t
        frame-title-format "%b"
        icon-title-format "%b"
        display-line-numbers-grow-only t
        auth-source-save-behavior nil
        uniquify-buffer-name-style 'forward)
  (setq-default mode-line-format
                '((:eval
                   (simple-mode-line-render
                    (format-mode-line
                     '((:eval meow--indicator)
                       " %* "
                       (:eval (jmpunkt/mode-line-buffer-name))
                       (:eval (jmpunkt/mode-line-position))
                       (:eval (jmpunkt/mode-line-region))))
                    (format-mode-line
                     '((:eval (jmpunkt/mode-line-file-info))
                       (:eval (jmpunkt/mode-line-vc))
                       (:eval (jmpunkt/mode-line-flymake))
                       (:eval (jmpunkt/mode-line-major-mode))))))))
  (set-face-attribute 'mode-line nil
                      :background (doom-color 'modeline-bg)
                      :foreground (doom-color 'modeline-fg)
                      :box `(:line-width 6 :color ,(doom-color 'modeline-bg))
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :background (doom-color 'modeline-bg-inactive)
                      :foreground (doom-color 'modeline-fg-inactive)
                      :box `(:line-width 6 :color ,(doom-color 'modeline-bg-inactive))
                      :overline nil
                      :underline nil)
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (toggle-scroll-bar -1)
  (global-so-long-mode 1)
  (electric-indent-mode -1)
  (pixel-scroll-precision-mode)

  (global-prettify-symbols-mode -1)
  (global-eldoc-mode -1))

(use-package eldoc
  :defer t
  :commands eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package paren
  :defer t
  :commands show-paren-mode
  :config
  (setq show-paren-delay 0))

(use-package ligature
  :defer t
  :commands ligature-mode
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

(use-package treesit
  :init
  (defvar jmpunkt/treesit-thing-lookup-table
    '((rust-mode . ((function . ((function_item block)))))
      ((js-mode js-jsx-mode ts-mode) . ((function . ((method_definition statement_block)
                                                     (arrow_function statement_block)))))
      (python-ts-mode . ((function . ((function_definition block))))))

    "A lookup table in the form of (mode . (( kind . ((inner1 outer1)
 (inner2 outer2)) )).

KIND is a chosen symbol which is describes the object.
INNER and OUTER are symbols which are defined by their grammar
definition.

Informal description: for a mode, it describes how to lookup a kind
where different syntax constructs describe the same kind. Each syntax
construct contains of an inner and outer, where inner must always be
contained by outer.")
  (defun jmpunkt/treesit--thing-for-mode (mode kind)
    (when-let* ((declarations (alist-get mode jmpunkt/treesit-thing-lookup-table nil nil
                                         (lambda (key mode) (if (sequencep key) (seq-contains key mode) (eq key mode))))))
      (alist-get kind declarations)))
  (defun jmpunkt/treesit--node-find-type (type &optional node)
    "Search for the closest treesit node with TYPE.

Either start at the current position or at NODE."
    (let ((current-node (or node (treesit-node-at (point)))))
      (while
          (and
           (not (treesit-node-eq (treesit-buffer-root-node) current-node))
           (not (string-equal (treesit-node-type current-node) type)))
          (setq current-node (treesit-node-parent current-node)))
      (when
          (and
           (not (treesit-node-eq (treesit-buffer-root-node) current-node))
           (string-equal (treesit-node-type current-node) type))
        current-node)))
  (defun jmpunkt/treesit-thing-at-point (kind &optional mode)
    "Return the OUTER and INNER node for the given KIND in MODE.

The INNER must be contained by the outer. See
`jmpunkt/trees-thing-lookup-table' for more information."
    (let ((definitions (jmpunkt/treesit--thing-for-mode (or mode major-mode) kind)))
      (cl-some (lambda (definition)
                 (if-let* ((outer-type (car definition))
                           (inner-type (cadr definition))
                           (inner-node (jmpunkt/treesit--node-find-type (symbol-name inner-type)))
                           (outer-node (jmpunkt/treesit--node-find-type (symbol-name outer-type) inner-node)))
                     `(,inner-node ,outer-node)))
               definitions))))

;;;; Shell
(use-package term
  :hook
  (term-mode . (lambda ()
                 (add-hook 'meow-insert-enter-hook #'term-char-mode nil t)
                 (add-hook 'meow-insert-exit-hook #'term-line-mode nil t))))
(use-package comint
  :bind (:map comint-mode-map
              ("C-j" . comint-next-matching-input-from-input)
              ("C-k" . comint-previous-matching-input-from-input))
  :init
  (defun jmpunkt/comint-goto-end-or-here ()
    "Smart comint goto promt.

Replicates the behavior of `jmpunkt/eshell-goto-end-or-here'."
    (interactive)
    (unless (comint-after-pmark-p)
      (let ((line (current-line)))
        (end-of-line)
        (if (comint-after-pmark-p)
            (comint-bol)
          (end-of-buffer)))))
  (define-advice meow-yank
      (:before (&rest args) comint-advice)
    (when (derived-mode-p 'comint-mode)
      (jmpunkt/comint-goto-end-or-here)))
  (define-advice comint-send-input
      (:before (&rest args) comint-advice)
      (jmpunkt/comint-goto-end-or-here))
  :config
  (defun jmpunkt/comint-goto-end-or-here-advice (&rest args)
    (jmpunkt/comint-goto-end-or-here))

  (advice-add 'comint-next-matching-input-from-input :before #'jmpunkt/comint-goto-end-or-here-advice)
  (advice-add 'comint-previous-matching-input-from-input :before #'jmpunkt/comint-goto-end-or-here-advice))

(use-package esh-mode
  :defer t
  :commands eshell-mode eshell
  :bind (:map eshell-mode-map
              ("C-j" . eshell-next-matching-input-from-input)
              ("C-k" . eshell-previous-matching-input-from-input))
  :hook
  (eshell-mode . (lambda ()
                   (add-hook 'meow-insert-enter-hook #'jmpunkt/eshell-goto-end-or-here nil t)))
  (eshell-before-prompt . (lambda ()
                            (setq-local xterm-color-preserve-properties t)))
  (eshell-pre-command . (lambda ()
                          (setq-local process-environment (copy-sequence process-environment))
                          (setenv "TERM" "xterm-256color")
                          (setenv "PAGER" "cat")))
  :init
  (defun jmpunkt/eshell-goto-end-or-here ()
    "Smart eshell goto promt.

If the cursor is in the history of eshell, then we want to go to the end of buffer.
If the cursor is on the promt of the eshell, then we want to go to the first writable position.
If the cursor is on the last promt, then we want to insert at the current position."
    (if (< (point) eshell-last-output-start)
        (end-of-buffer)
      (let ((pos (point)))
        (progn
          (while (get-text-property pos 'read-only)
            (setq pos (+ pos 1)))
          (when (not (eq (point) pos))
            (goto-char pos))))))
  (defun jmpunkt/eshell-goto-end-or-here-advice (&rest args)
    (jmpunkt/eshell-goto-end-or-here))
  ;; Before history selection, goto the insertion line of the
  ;; shell. This way the search string for history selection will not
  ;; select anything in the previous output.
  (advice-add 'eshell-next-matching-input-from-input :before #'jmpunkt/eshell-goto-end-or-here-advice)
  (advice-add 'eshell-previous-matching-input-from-input :before #'jmpunkt/eshell-goto-end-or-here-advice)
  (define-advice meow-yank
      (:before (&rest args) eshell-advice)
    (when (derived-mode-p 'eshell-mode)
      (jmpunkt/eshell-goto-end-or-here)))
  (define-advice eshell-send-input
      (:before (&rest args) eshell-advice)
    (jmpunkt/eshell-goto-end-or-here))
  :config
  (require 'xterm-color)
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq eshell-history-size 10000
        eshell-hist-ignoredups t))

(use-package compile
  :defer t
  :commands compilation-mode
  :config
  (require 'xterm-color)
  (setq compilation-scroll-output t)
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun jmpunkt/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'jmpunkt/advice-compilation-filter))

;;; * Core Packages
;;;; * helpful
(use-package helpful
  :hook (helpful-mode . button-mode)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (:map global-map
              ("C-h F" . helpful-callable)
              ("C-h f" . helpful-function)
              ("C-h v" . helpful-variable)
              ("C-h k" . helpful-key)))

;;;; * undo-tree
(use-package undo-tree
  :defer t
  :commands undo-tree-mode
  :hook ((prog-mode . undo-tree-mode)
         (conf-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil))

;;;; * Evil
(use-package meow
  :demand t
  :bind (:map global-map
              ("M-." . jmpunkt/meow-find-definitions)
              ("M-?" . jmpunkt/meow-find-references)
              ("C-d" . meow-page-down)
              ("C-u" . meow-page-up))
  :init
  (defun jmpunkt/meow--bound-of-treesit (symbol)
    (if-let ((node (cadr (jmpunkt/treesit-thing-at-point symbol))))
        `(,(treesit-node-start node) . ,(treesit-node-end node))))
  (defun jmpunkt/meow--inner-of-treesit (symbol)
    (if-let ((node (car (jmpunkt/treesit-thing-at-point symbol))))
        `(,(treesit-node-start node) . ,(treesit-node-end node))))
  (defun jmpunkt/meow--bound-of-function ()
    (jmpunkt/meow--bound-of-treesit 'function))
  (defun jmpunkt/meow--inner-of-function ()
    (jmpunkt/meow--inner-of-treesit 'function))
  (defun jmpunkt/meow-find-references ()
    "Xref definition."
    (interactive)
    (meow--cancel-selection)
    (smart-jump-references))
  (defun jmpunkt/meow-find-definitions ()
    "Xref definition."
    (interactive)
    (meow--cancel-selection)
    (smart-jump-go))
  (defun jmpunkt/meow-setup ()
    (meow-thing-register 'function #'jmpunkt/meow--inner-of-function #'jmpunkt/meow--bound-of-function)
    (add-to-list 'meow-char-thing-table '(?f . function))
    (setq meow-replace-state-name-list
          '((normal . "N")
            (motion . "M")
            (keypad . "K")
            (insert . "I")
            (beacon . "B")
            (selection . "S")))
    ;; setup basic selection mode
    (setq meow-selection-keymap (make-keymap))
    (meow-define-state selection
      "meow state for basic moving and selection operations"
      :lighter " [S]"
      :keymap meow-selection-keymap)
    (setq meow-cursor-type-selection 'box)
    (meow-define-keys 'selection
      '("j" . meow-next)
      '("k" . meow-prev)
      '("h" . meow-left)
      '("l" . meow-right)
      '("x" . meow-line)
      '(";" . meow-reverse)
      '("," . meow-inner-of-thing)
      '("." . meow-bounds-of-thing))
    (add-to-list 'meow-mode-state-list '(magit-status-mode . selection))
    (add-to-list 'meow-mode-state-list '(magit-revision-mode . selection))
    (add-to-list 'meow-replace-state-name-list '(selection . "SELECTION"))
    (add-to-list 'meow-indicator-face-alist '(selection . meow-normal-indicator))
    ;; default meow setup
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
          meow-use-clipboard t
          meow-expand-hint-remove-delay 5.0
          meow-selection-command-fallback'((meow-kill . meow-C-k)
                                           (meow-cancel-selection . keyboard-quit)
                                           (meow-pop-selection . meow-pop-grab)
                                           (meow-beacon-change . meow-beacon-change-char)))
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("h" . meow-left)
     '("l" . meow-right)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("(" . meow-beginning-of-thing)
     '(")" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . delete-char)
     '("D" . backward-delete-char)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("p" . meow-yank)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)
     '("C-r" . undo-tree-redo)
     '("=" . indent-region)))
  :config
  (jmpunkt/meow-setup)
  (meow-global-mode 1))

;;;; * Dired
(use-package dired
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

;;;; * DirEnv
(use-package envrc
  :bind-keymap ("C-c e" . envrc-command-map)
  :hook ((prog-mode . envrc-mode)
         (org-mode . envrc-mode)
         (eshell-mode . envrc-mode)
         (comint-mode . envrc-mode)))

;;;; * Which-Key
(use-package which-key
  :hook (after-init . which-key-mode))

;;;; * xref/jumping
(use-package smart-jump
  :demand t
  :bind (("M-." . smart-jump-go)
         ("M-," . smart-jump-back)
         ("M-?" . smart-jump-references)))

;;;; * Spelling
(use-package ispell
  :defer t
  :config
  (setq-default ispell-program-name "aspell"
                ;; Hide all default entries which may not be available
                ;; on the system anyways
                ispell-extra-args '("--sug-mode=ultra"
                                    "--run-together")
                ispell-dictionary-base-alist nil
                ispell-local-dictionary "en"))

(use-package flyspell
  :defer t
  :commands flyspell-mode
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
  :hook ((prog-mode . (lambda ()
                        (when (jmpunkt/flyspell-enabled-for-mode)
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

(use-package eglot-ltex
  :ensure t)

;;;; * Dashboard
(use-package dashboard
  :hook (after-init . dashboard-refresh-buffer)
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
  :defer t
  :commands eglot-ensure
  :bind (:map eglot-mode-map
              ("C-c k r" . jmpunkt/eglot-rename)
              ("C-c C-f" . eglot-format-buffer)
              ("M-RET" . jmpunkt/eglot-code-actions))
  :init
  (defun jmpunkt/eglot-rename ()
    (interactive)
    (meow--cancel-selection)
    (call-interactively 'eglot-rename))
  (defun jmpunkt/eglot-code-actions ()
    (interactive)
    (meow--cancel-selection)
    (call-interactively 'eglot-code-actions))
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
  (defun jmpunkt/eglot-disable-mouse()
    ;; disable mouse support for flymake face
    (cl-loop for i from 1
             for type in '(eglot-note eglot-warning eglot-error)
             do (put type 'flymake-overlay-control
                     `((priority . ,(+ 50 i))
                       (face . flymake-error)))))
  :config
  (setq eglot-extend-to-xref t)
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'highlight)

  (jmpunkt/eglot-disable-mouse)
  (setq eglot-stay-out-of '(company)
        eglot-confirm-server-initiated-edits nil))

(use-package eglot-x
  :after eglot)

;;;; * Flymake
(use-package flymake
  :after consult
  :commands flymake-mode
  :bind (:map flymake-mode-map
              ([f7] . consult-flymake))
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          " " flymake-mode-line-warning-counter
          " " flymake-mode-line-note-counter
          " ")))

;;;; RSS
(use-package elfeed
  :defer t
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
                              (".*raw\.githubusercontent.*" . browse-url-emacs)
                              (".*github.*" . browse-url-firefox)
                              ("." . eww-browse-url))))
(use-package eww
  :defer t
  :commands (eww eww-follow-link)
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html?q="))

;;;; Search/Find
(use-package minibuffer
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (minibuffer-setup . minibuffer-depth-indicate-mode))
  :bind (:map minibuffer-local-map
              ([remap keyboard-quit] . minibuffer-keyboard-quit)
              ("<escape>" . minibuffer-exit)
              ("<return>" . exit-minibuffer)
              ("\t" . minibuffer-force-complete)
              ("C-j" . next-line)
              ("C-k" . previous-line)
              ("M-j" . scroll-up-command)
              ("M-k" . scroll-down-command)
              ("C-S-j" . forward-paragraph)
              ("C-S-k" . backward-paragraph)
              ("C-b" . beginning-of-buffer)
              ("C-e" . end-of-buffer)
              ("C-n" . next-history-element)
              ("C-m" . previous-history-element))
  :config
  (setq minibuffer-local-map (make-sparse-keymap)
        minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        enable-recursive-minibuffers t
        completion-ignore-case t))

;; This setup comes close to vertico. However, it is not perfect and
;; the following things are missing:
;; - groups (not using it anyways?)
;; - highlight whole line
;; - marginalia looks a bit strange?
;; - when typing all candidates are hidden (but counted) -> echo area infers
(use-package icomplete
  :hook ((icomplete-minibuffer-setup . (lambda () (setq truncate-lines t)))
         ;;(after-init . icomplete-vertical-mode)
         )
  :config
  (setq icomplete-scroll t)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-compute-delay 0.05)
  (set-face-attribute 'icomplete-selected-match nil :inherit 'region)
  ;; skip all icomplete maps and inherit directly from minibuffer
  (setq icomplete-vertical-mode-minibuffer-map (make-composed-keymap nil minibuffer-local-map))
  :bind (:map icomplete-vertical-mode-minibuffer-map
              ("<return>". icomplete-force-complete-and-exit)
              ([remap minibuffer-force-complete-and-exit] . icomplete-force-complete-and-exit)
              ([remap minibuffer-force-complete] . icomplete-force-complete)
              ([remap next-line] . icomplete-forward-completions)
              ([remap previous-line] . icomplete-backward-completions)))

(use-package embark
  :bind (("C-c C-r" . embark-act)
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
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :demand t
  :init
  (defun jmpunkt/flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun jmpunkt/literal-if-eq (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-style-dispatchers '(jmpunkt/flex-if-twiddle jmpunkt/literal-if-eq)
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

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
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (or vertico-mode icomplete-vertical-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (consult-customize
   affe-grep affe-find
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-.")))

(use-package recentf
  :hook (after-init . recentf-mode)
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

(use-package cape
  :demand t
  :bind (:map global-map
              ("C-c c p" . completion-at-point)
              ("C-c c t" . complete-tag)
              ("C-c c d" . cape-dabbrev)
              ("C-c c f" . cape-file)
              ("C-c c h" . cape-history)
              ("C-c c k" . cape-keyword)
              ("C-c c s" . cape-symbol)
              ("C-c c a" . cape-abbrev)
              ("C-c c i" . cape-ispell)
              ("C-c c l" . cape-line)
              ("C-c c w" . cape-dict)
              ("C-c c _" . cape-tex)
              ("C-c c &" . cape-sgml)
              ("C-c c r" . cape-rfc1345))
  :init
  (defalias 'cape-symbol+dabbrev
    (cape-super-capf #'cape-symbol #'cape-dabbrev))
  (defun jmpunkt/cape-setup-git-commit ()
    (add-to-list 'completion-at-point-functions 'cape-dabbrev))
  (defun jmpunkt/cape-setup-eshell ()
    (add-to-list 'completion-at-point-functions 'cape-file))
  (defun jmpunkt/cape-setup-elisp ()
    (add-to-list 'completion-at-point-functions 'cape-symbol+dabbrev))
  (defun jmpunkt/cape-setup-comint ()
    (add-to-list 'completion-at-point-functions 'cape-symbol+dabbrev))
  :hook ((git-commit-mode . jmpunkt/cape-setup-git-commit)
         (eshell-mode . jmpunkt/cape-setup-eshell)
         (emacs-lisp-mode . jmpunkt/cape-setup-elisp))
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (setq cape-dabbrev-min-length 3))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;;;; * Git
(use-package magit
  :bind ((:map global-map
               ("C-x g" . magit-status)
               ("C-x G" . magit-status-here))
         (:map magit-status-mode-map
               ("t" . magit-discard)
               ("T" . magit-tag)))
  :config
  (setq magit-slow-confirm '(magit-discard))
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
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
  :hook (after-init . global-hl-todo-mode))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)))

;;;; * Org
(use-package org
  :defer t
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
              ("C-c C-f" . jmpunkt/format-org-src)
              ("C-j" . nil))
  :hook ((org-mode . (lambda ()
                       (setq-local tab-width 2)
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

(use-package ob-shell
  :after org
  :defer t
  :commands (org-babel-execute:shell))

(use-package ob-graphql
  :after org
  :defer t
  :commands (org-babel-execute:graphql))

(use-package ob-sql
  :after org
  :defer t
  :commands (org-babel-execute:sql))

(use-package ob-sqlite
  :after org
  :defer t
  :commands (org-babel-execute:sqlite))

(use-package ob-python
  :after org
  :defer t
  :commands (org-babel-execute:python))

(use-package ob-latex
  :after org
  :defer t
  :commands (org-babel-execute:latex
             org-babel-expand-body:latex))

(use-package ob-gnuplot
  :after org
  :defer t
  :commands (org-babel-expand-body:gnuplot))

(use-package ob-dot
  :after org
  :defer t
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package ob-emacs-lisp
  :defer t
  :after org
  :commands (org-babel-execute:emacs-lisp
             org-babel-expand-body:emacs-lisp))

(use-package ob-plantuml
  :after org
  :defer t
  :commands (org-babel-execute:plantuml))

(use-package ob-async
  :after org
  :commands (ob-async-org-babel-execute-src-block
             org-babel-execute-src-block:async))

(use-package org-agenda
  :after org
  :defer t
  :config
  (setq org-agenda-files (list org-agenda-dir)))

(use-package org-capture
  :defer t
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
  :defer t
  :after org
  :hook (org-mode . org-indent-mode))

(use-package org-src
  :after org
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
  :after org
  :defer t
  :config
  (setq org-latex-listings 'minted
        org-latex-prefer-user-labels t
        org-latex-packages-alist '(("" "minted") ("" "xcolor") ("" "listings") ("" "url"))
        org-latex-compiler "xelatex"
        org-latex-pdf-process '("latexmk -g -pdf -pdflatex=\"%latex -synctex=1 -shell-escape -interaction=nonstopmode\" -outdir=%o %f"))
  (add-to-list
   'org-latex-classes
   '("informal"
     "\\documentclass{scrartcl}
      \\usepackage[parfill]{parskip}
      \\pagenumbering{gobble}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
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
     "\\documentclass{scrartcl}
      \\usepackage[parfill]{parskip}"
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
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("●" "○" "✸" "✿")))

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset)))

;;; * Configuration Files
;;;; * Mermaid
(use-package mermaid-mode
  :defer t)

;;;; * YAML
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . eglot-ensure)
  :mode ("\\.yaml\\'" "\\.yml\\'"))

;;;; * Conf{TOML}
(use-package conf-mode
  :defer t
  :mode (("\\.toml\\'" . conf-toml-mode))
  :hook (conf-toml-mode . conf-toml-mode-treesit-setup)
  :init
  (defvar conf-toml-mode--treesit-settings
    (treesit-font-lock-rules
     :language 'toml
     :feature 'table-key
     '((table [(quoted_key) (dotted_key) (bare_key)] @font-lock-type-face))
     :language 'toml
     :feature 'bare-key
     '((bare_key) @font-lock-variable-name-face)
     :language 'toml
     :feature 'quoted-key
     '((quoted_key) @font-lock-variable-name-face)
     :language 'toml
     :feature 'boolean
     '((boolean) @font-lock-constant-face)
     :language 'toml
     :feature 'comment
     '((comment) @font-lock-comment-face)
     :language 'toml
     :feature 'string
     '((string) @font-lock-string-face)
     :language 'toml
     :feature 'integer
     '((integer) @font-lock-constant-face)
     :language 'toml
     :feature 'float
     '((float) @font-lock-constant-face)
     :language 'toml
     :feature 'offset-date-time
     '((offset_date_time) @font-lock-constant-face)
     :language 'toml
     :feature 'local-date-time
     '((local_date_time) @font-lock-constant-face)
     :language 'toml
     :feature 'local-date
     '((local_date) @font-lock-constant-face)
     :language 'toml
     :feature 'local-time
     '((local_time) @font-lock-constant-face)
     ))
  (defun conf-toml-mode-treesit-setup ()
    (require 'treesit)
    (cond
     ((treesit-ready-p 'conf-toml-mode 'toml)
      (setq-local treesit-font-lock-feature-list
                  '((comment string table-key quoted-key bare-key)
                    (float integer string boolean)
                    (offset-date-time local-date-time local-date local-time)))
      (setq-local treesit-font-lock-settings conf-toml-mode--treesit-settings)
      (treesit-major-mode-setup)))))

;;;; * GraphQL
(use-package graphql-mode
  :defer t
  :bind (:map graphql-mode-map
              ("C-c C-f" . prettier-js))
  :mode ("\\.graphql\\'"))

;;; * Text Files
;;;; * reStructuredText
(use-package rst
  :defer t
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;;;; * Markdown
(use-package markdown-mode
  :defer t
  :mode
  (("INSTALL\\.md\\\'". gfm-mode)
   ("CONTRIBUTORS\\.md\\\'". gfm-mode)
   ("LICENSE\\.md\\\'". gfm-mode)
   ("README\\.md\\\'". gfm-mode)
   ("README\\.md\\'". gfm-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

;;;; * Graphivz
(use-package graphviz-dot-mode
  :defer t
  :mode "\\.dot\\'")

;;; * Programming Languages

;;;; * SQL
(use-package sql
  :defer t
  :mode ("\\.sql\\'" . sql-mode)
  :hook (sql-mode . (lambda () (setq-local tab-width 2)))
  :config
  (sql-highlight-postgres-keywords))

(use-package sqlformat
  :after sql
  :defer t
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat-buffer))
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g")))

;;;; * Haskell
(use-package haskell-mode
  :defer t
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure))

;;;; * Nix
(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :hook ((nix-mode . eglot-ensure)
         (nix-mode . (lambda ()
                       (setq-local tab-width 2))))
  :bind ((:map nix-mode-map
               ("C-c C-f" . nix-format-buffer))
         (:map jmpunkt/eglot-keys-map
               ("C-c C-f" . nix-format-buffer)))
  :init
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
  :defer t
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (python-base-mode . eglot-ensure)
  :config (setq python-indent-offset 4))

;;;; * Rust
(use-package rust-mode
  :hook (rust-mode . eglot-ensure)
  :defer t
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c k t" . rust-test)
              ("C-c k r" . rust-run)
              ("C-c k c" . rust-check)
              ("C-c k C" . rust-clippy)
              ("C-c k b" . rust-compile)
              ("C-c C-f" . rust-format-buffer)))

;;;; * Fish
(use-package fish-mode
  :defer t
  :mode "\\.fish\\'")

;;;; * WEB
(use-package sgml-mode
  :defer t
  :mode ("\\.html\\'" . html-mode)
  :hook ((html-mode . eglot-ensure)
         (html-mode . prettier-js-mode)))

(use-package css-mode
  :defer t
  :mode (("\\.css\\'" . css-ts-mode)
         ("\\.scss\\'" . scss-mode))
  :hook ((css-base-mode . eglot-ensure)
         (css-base-mode . prettier-js-mode)
         (scss-base-mode . eglot-ensure)
         (scss-mode . prettier-js-mode))
  :bind (:map css-base-mode-map
              ("C-c C-f" . prettier-js))

  :config
  (setq css-indent-offset 2))

(use-package typescript-ts-mode
  :hook ((typescript-ts-base-mode . eglot-ensure)
         (typescript-ts-base-mode . prettier-js-mode))
  :bind (:map typescript-ts-base-mode-map
              ("C-c C-f" . prettier-js))
  :config
  (add-to-list 'eglot-server-programs '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio"))))

(use-package js
  :defer t
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-jsx-mode)
         ("\\.json\\'" . js-json-mode)
         ("\\.jsonc\\'" . js-json-mode))
  :hook ((js-base-mode . eglot-ensure)
         (js-base-mode . prettier-js-mode))
  :init
  :bind (:map js-base-mode-map
              ("C-c C-f" . prettier-js))
  :config
  (setq js-indent-level 2))

(use-package prettier-js
  :defer t
  :commands prettier-js)

;;;; * LaTeX
(use-package tex-mode
  :defer t
  :mode ("\\.tex\\'" . latex-mode))

;;; * PDF
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (pdf-misc-size-indication-minor-mode)
                           (pdf-links-minor-mode)
                           (pdf-isearch-minor-mode)
                           (cua-mode -1)))
  :bind (:map pdf-view-mode-map
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

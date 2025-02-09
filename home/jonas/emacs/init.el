;;; init.el --- Initilizes Emacs -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
(require 'bind-key)
(require 'nixos-paths)

(defcustom presentation-mode-font-height 200
  "Store previous face configuration."
  :type 'number
  :group 'presentation)

(defvar presentation-mode--previous nil
  "Store previous face configuration.")

(define-minor-mode presentation-mode
  "Toggles global `presentation-mode'."
  :init-value nil
  :global t
  :group 'presentation
  (if presentation-mode
      (progn
        (setq presentation-mode--previous (face-all-attributes 'default))
        (set-face-attribute 'default nil
                            :weight 'regular
                            :width 'normal
                            :height presentation-mode-font-height))
    (apply 'set-face-attribute
           'default nil
           (seq-mapcat (lambda (x) (when (and
                                          (not (equal :foundry (car x)))
                                          (not (equal :family (car x))))
                                     (list (car x) (cdr x))))
                       presentation-mode--previous))
    (setq presentation-mode--previous nil)))

(use-package xdg-appmenu
  :commands xdg-appmenu
  :init
  (defun emacs-run-launcher ()
    (interactive)
    (with-selected-frame (make-frame '((name . "emacs-run-launcher")
                                       (minibuffer . only)
                                       (auto-raise . t)
                                       (fullscreen . nil)
                                       (undecorated . t)
                                       (internal-border-width . 20)
                                       (width . 200)
                                       (height . 11)))
      (unwind-protect
          (call-interactively #'xdg-appmenu)
        (delete-frame)))))

(use-package gcmh-mode
  :hook (emacs-startup . gcmh-mode))

;;; * Use-package (Formatting)
(defcustom fmt-formatters-alist '()
  "List of available formatters for major-modes."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'fmt)

(defcustom fmt-mode-map (define-keymap
                          "C-c C-f" #'fmt-format-buffer)
  "Keymap for `fmt-mode'."
  :type 'keymap
  :group 'fmt)

(define-minor-mode fmt-mode
  "Minor mode for formatting with fallbacks."
  :init-value nil
  :lighter " Fmt"
  :keymap fmt-mode-map)

(defun fmt-format-buffer ()
  "General formatting function for `fmt-mode'.

Use formatter function defined in `fmt-formatters-alist' if major mode
or its parent modes are contained in that list.  Otherwise, fallback to
eglot (if available)."
  (interactive)
  (let* ((mode (seq-find (lambda (mode) (assoc mode fmt-formatters-alist))
                         (derived-mode-all-parents major-mode)))
         (formatter (cdr (assoc mode fmt-formatters-alist))))
    (if formatter
        (funcall formatter)
      (when (and (seq-contains minor-mode-list 'eglot--managed-mode) (eglot-current-server))
        (eglot-format-buffer)))))

(defun use-package-normalize/:fmt (name keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((consp arg)
        (cond
         ((symbolp (car arg))
          (cond
           ((symbolp (cdr arg)) arg)
           (t
            (use-package-error
             ":fmt wants a symbol in the second position"))))
         (t
          (use-package-error
           ":fmt wants a symbol in the first position"))))
       (t
        (use-package-error
         ":fmt wants a cons (like `(symbol . symbol)')"))))))

(defun use-package-handler/:fmt (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((add-to-list 'fmt-formatters-alist ',arg)))))

(add-to-list 'use-package-keywords :fmt)

;;; * Theme
(use-package modus-themes
  :init
  (load-theme 'modus-vivendi-tinted t)
  (set-face-attribute 'line-number-current-line nil :box nil))

;;; * Emacs
(use-package emacs
  :hook ((prog-mode . jmpunkt/prog-init)
         (prog-mode . fmt-mode)
         (conf-mode . jmpunkt/conf-init)
         (text-mode . jmpunkt/text-init))
  :bind (:map global-map
              ("C-+" . text-scale-increase)
              ("C--" . text-scale-decrease)
              ("M-C-;" . eval-expression)
              ("TAB" . completion-at-point)
              ("C-j" . jmpunkt/join-line)
              ("M-N" . shell-command)
              ("M-S-<up>" . async-shell-command)
              ("C-g" . prot/keyboard-quit-dwim))
  :init
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))
  (defun xdg-open-file ()
    "In dired or buffer, open the file named on this line."
    (interactive)
    (call-process "xdg-open" nil 0 nil (or
                                        (when (eq major-mode 'dired-mode)
                                          (dired-get-filename nil t)
                                          (dired-current-directory))
                                        buffer-file-name)))
  (defun jmpunkt/join-line ()
    (interactive)
    (next-line)
    (delete-indentation))
  (defun jmpunkt/default-init ()
    (subword-mode 1)
    (electric-indent-local-mode 1)
    (display-fill-column-indicator-mode 1)
    (show-paren-mode 1)
    (display-line-numbers-mode 1)
    (setq-local show-trailing-whitespace t))
  (defun jmpunkt/prog-init ()
    (corfu-mode)
    (jmpunkt/default-init))
  (defun jmpunkt/conf-init ()
    (corfu-mode)
    (jmpunkt/default-init))
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
        (propertize " %b " 'face 'error)
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
  (define-advice eglot--mode-line-format
      (:override () jmpunkt)
    (let* ((server (eglot-current-server))
           (nick (and server (eglot-project-nickname server)))
           (pending (and server (jsonrpc-continuation-count server)))
           (last-error (and server (jsonrpc-last-error server)))
           (is-pending (cl-plusp pending))
           (is-error (or last-error nil))
           (reports (eglot--progress-reporters server))
           (has-reports (cl-plusp (hash-table-count reports))))
      (append
       `(,(propertize "eglot" 'face 'eglot-mode-line))
       (when (or is-error is-pending has-reports)
         `(":"
           ,@(when is-error
               `(,(propertize "error" 'face 'compilation-mode-line-fail)))
           ,@(when is-pending
               `(,(when is-error "/") ,(propertize (format "%d" pending) 'face 'warning)))
           ,@(cl-loop for pr hash-values of reports
                      when (eq (car pr)  'eglot--mode-line-reporter)
                      append `(,(when (or is-error is-pending) "/")
                               ,(propertize
                                 (format "[%s]" (or (nth 4 pr) "?"))
                                 'face 'eglot-mode-line)
                               ,(propertize
                                 (format "%s" (or (nth 3 pr) (nth 2 pr) (nth 1 pr)))
                                 'face 'eglot-mode-line))))))))
  (setq indent-line-function 'indent-relative
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
        uniquify-buffer-name-style 'forward
        history-delete-duplicates t
        xref-search-program 'ripgrep)
  (setq-default tab-width 2
                fill-column 80
                indent-tabs-mode nil
                comment-auto-fill-only-comments t)
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
                       (:eval (jmpunkt/mode-line-major-mode))
                       (:eval mode-line-misc-info)))))))
  (set-face-attribute 'mode-line nil
                      :height 125)
  (set-face-attribute 'default nil
                      :weight 'regular
                      :width 'normal
                      :height 125)
  (set-face-attribute 'variable-pitch nil :inherit 'default :family 'unspecified)
  (set-face-attribute 'fixed-pitch nil :inherit 'default :family 'unspecified)
  (save-place-mode 1)
  (savehist-mode 1)
  (global-hl-line-mode 1)
  (global-so-long-mode 1)
  (electric-indent-mode -1)
  (electric-pair-mode -1)
  (pixel-scroll-precision-mode -1)

  (global-prettify-symbols-mode -1)
  (global-eldoc-mode -1)
  (put 'narrow-to-region 'disabled nil))

(use-package tempel
  :bind (:map tempel-map
              ("M-n" . tempel-next)))

(use-package eldoc
  :commands eldoc-mode
  :bind (:map global-map
              ("C-c d" . eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-prefer-doc-buffer t))

(use-package paren
  :commands show-paren-mode
  :config
  (setq show-paren-delay 0))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;;; Shell
(use-package eat
  :commands eat)

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
  (define-advice comint-next-matching-input-from-input
      (:before (&rest args) comint-advice)
    (jmpunkt/comint-goto-end-or-here))
  (define-advice comint-previous-matching-input-from-input
      (:before (&rest args) comint-advice)
    (jmpunkt/comint-goto-end-or-here)))

(use-package esh-mode
  :commands eshell-mode eshell
  :hook
  (eshell-mode . eshell-hist-mode)
  (eshell-mode . (lambda ()
                   (add-hook 'meow-insert-enter-hook #'jmpunkt/eshell-goto-end-or-here nil t)))
  (eshell-before-prompt . (lambda ()
                            (setq-local xterm-color-preserve-properties t)))
  (eshell-pre-command . (lambda ()
                          (setq-local process-environment (copy-sequence process-environment))
                          (setenv "TERM" "xterm-256color")
                          (setenv "PAGER" "cat")
                          (setenv "MANPAGER" "cat")))
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
  ;; Before history selection, goto the insertion line of the
  ;; shell. This way the search string for history selection will not
  ;; select anything in the previous output.
  (define-advice eshell-next-matching-input-from-input
      (:before (&rest args) eshell-advice)
    (jmpunkt/eshell-goto-end-or-here))
  (define-advice eshell-previous-matching-input-from-input
      (:before (&rest args) eshell-advice)
    (jmpunkt/eshell-goto-end-or-here))
  (define-advice meow-yank
      (:before (&rest args) eshell-advice)
    (when (derived-mode-p 'eshell-mode)
      (jmpunkt/eshell-goto-end-or-here)))
  (define-advice eshell-send-input
      (:before (&rest args) eshell-advice)
    (jmpunkt/eshell-goto-end-or-here))
  :config
  (require 'xterm-color)
  (add-to-list 'eshell-preoutput-filter-functions 'jmpunkt/xterm-color-propertized-filter)
  (defun jmpunkt/strip-properties (propertized-string)
    "Strips all properties of a string."
    (let* ((string (substring propertized-string))
           (start 0)
           (end (length string)))
      (set-text-properties start end nil string)
      string))
  (defun jmpunkt/xterm-color-propertized-filter (string)
    "Filter propertized strings with xterm-color-filter."
    (let* ((filtered (xterm-color-filter (jmpunkt/strip-properties string)))
           (new-length (length filtered)))
      (cl-loop for prop in (object-intervals string)
               do
               (add-text-properties (car prop)
                                    (min (cadr prop) new-length)
                                    (caddr prop)
                                    filtered))
      filtered))
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq eshell-history-size 1000
        eshell-history-append t
        eshell-hist-ignoredups 'erase))

(use-package compile
  :commands compilation-mode
  :config
  (setq compilation-scroll-output t)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  ;; TODO: xterm-color and rg.el are not compatible
  ;; (require 'xterm-color)
  ;; (setq compilation-environment '("TERM=xterm-256color"))
  ;; (define-advice compilation-filter
  ;;     (:around (f proc string) xterm-color-advice)
  ;;   (funcall f proc (xterm-color-filter string)))
  )

;;; * Core Packages
(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
    scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

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
              ("C-h M" . helpful-macro)
              ("C-h k" . helpful-key)))

;;;; * undo-tree
(use-package undo-tree
  :commands undo-tree-mode
  :hook ((prog-mode . undo-tree-mode)
         (conf-mode . undo-tree-mode)
         (text-mode . undo-tree-mode))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history nil))

;;;; * Avy
(use-package avy
  :custom
  (avy-timeout-seconds 0.1)
  :config
  (avy-setup-default))

(use-package casual-avy
  :commands casual-avy-tmenu)

;;;; * Meow
(use-package meow
  :demand t
  :bind (:map global-map
              ("C-d" . meow-page-down)
              ("C-u" . meow-page-up))
  :init
  (defun jmpunkt/meow-block-or-parent (arg)
    "Call meow-block or jmpunkt/meow-treesit-parent if treesit is supported."
    (interactive "P")
    (if (and (featurep 'treesit) (treesit-language-at (point)))
        (call-interactively 'jmpunkt/meow-treesit-parent)
      (call-interactively 'meow-block)))
  (defun jmpunkt/meow-treesit-parent (arg)
    "Mark the current node or expand to parent node."
    (interactive "P")
    (unless (equal 'parent (cdr (meow--selection-type)))
      (meow--cancel-selection))
    (let* ((back (xor (meow--direction-backward-p) (< (prefix-numeric-value arg) 0)))
           (node (if (meow--selection-type)
                     (treesit-node-on (region-beginning) (region-end))
                   (treesit-node-at (point))))
           (target (if (meow--selection-type) (treesit-node-parent node) node)))
      ;; If the parent does not expand the current selection, then use their
      ;; parent instead.
      (while (and
              (meow--selection-type)
              (equal (treesit-node-end target) (treesit-node-end node))
              (equal (treesit-node-start target) (treesit-node-start node))
              (treesit-node-parent target))
        (setq target (treesit-node-parent target)))
      (let ((m (if back (treesit-node-end target) (treesit-node-start target)))
            (p (if back (treesit-node-start target) (treesit-node-end target))))
        (when (and m p)
          (thread-first
            (meow--make-selection '(expand . parent) m p)
            (meow--select))
          (meow--maybe-highlight-num-positions)))))
  (defun jmpunkt/meow-search-with (search)
    (interactive (list (read-string "search: " nil 'regexp-search-ring)))
    (when search
      (push search regexp-search-ring)
      (call-interactively #'meow-search)))
  (defun jmpunkt/meow-setup ()
    ;; add treesit function alias
    (add-to-list 'meow-char-thing-table '(?f . defun))
    ;; add thing-at-point url
    (meow-thing-register 'url 'url 'url)
    (add-to-list 'meow-char-thing-table '(?u . url))
    ;; add angle brackets
    (meow-thing-register 'angle
                         '(pair ("<") (">"))
                         '(pair ("<") (">")))
    (add-to-list 'meow-char-thing-table '(?a . angle))
    ;; change display name of modes
    (setq meow-replace-state-name-list
          '((normal . "N")
            (motion . "M")
            (keypad . "K")
            (insert . "I")
            (beacon . "B")))
    (add-to-list 'meow-mode-state-list '(compilation-mode . normal))
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
     '("x" . meow-line)
     '("y" . meow-save)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("(" . meow-beginning-of-thing)
     '(")" . meow-end-of-thing)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC <char> will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     '("x" . "H-x")
     '("y" . "H-y")
     '("," . "H-,")
     '("." . "H-.")
     '("(" . "H-(")
     '(")" . "H-)")
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
     '("n" . meow-search)
     '("N" . jmpunkt/meow-search-with)
     '("o" . jmpunkt/meow-block-or-parent)
     '("O" . meow-to-block)
     '("g" . meow-till)
     '("G" . meow-grab)
     '("p" . meow-yank)
     '("q" . meow-goto-line)
     '("Q" . casual-avy-tmenu)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-cancel-selection)
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
     '("<" . indent-rigidly-left-to-tab-stop)
     '(">" . indent-rigidly-right-to-tab-stop)
     '("<escape>" . ignore)
     '("C-r" . undo-tree-redo)
     '("=" . meow-indent)))
  :config
  (jmpunkt/meow-setup)
  (meow-global-mode 1))

;;;; * Formatter
(use-package reformatter
  :init
  (reformatter-define fmt/alejandra
    :program "alejandra"
    :args (list input-file)
    :stdin nil
    :stdout nil
    :input-file (reformatter-temp-file-in-current-directory)
    :group 'nix)
  (reformatter-define fmt/nixpkgs-fmt
    :program "nixpkgs-fmt"
    :group 'nix)
  (reformatter-define fmt/ruff
    :program "ruff"
    :args (list "format" "--stdin-filename" (or (buffer-file-name) (buffer-name)))
    :group 'python)
  (reformatter-define fmt/pgformatter
    :program "pg_format"
    :group 'sql)
  (reformatter-define fmt/typstyle
    :program "typstyle"
    :group 'typst)
  (reformatter-define fmt/biome
    :program "biome"
    :args (list "format" "--stdin-file-path" (or (buffer-file-name) (buffer-name)))
    :group 'web)
  (reformatter-define fmt/prettier
    :program "prettier"
    :args (list "--stdin-filepath" (or (buffer-file-name) (buffer-name)))
    :group 'web))

;;;; * Dired
(use-package dired
  :config
  (setq dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t))

(use-package transient-dwim
  :bind ("C-x h" . transient-dwim-dispatch))

;;;; * DirEnv
(use-package envrc
  :hook ((prog-mode . envrc-mode)
         (org-mode . envrc-mode)
         (eshell-mode . envrc-mode)
         (comint-mode . envrc-mode)))

;;;; * Which-Key
(use-package which-key
  :hook (after-init . which-key-mode))

;;;; * xref/jumping
(use-package citre
  :bind (("C-c m b" . citre-jump-back)
         ("C-c m f" . jmpunkt/meow-find-definitions)
         ("C-c m r" . jmpunkt/meow-find-references)
         ("C-c m c" . citre-create-tags-file)
         ("C-c m u" . citre-update-tags-file))
  :custom
  (citre-default-create-tags-file-location 'global-cache)
  :init
  (defun jmpunkt/meow-find-references ()
    "Xref definition."
    (interactive)
    (meow--cancel-selection)
    (citre-jump-to-reference))
  (defun jmpunkt/meow-find-definitions ()
    "Xref definition."
    (interactive)
    (meow--cancel-selection)
    (citre-jump))
  :config
  (require 'citre-config))


;;;; * Spelling
(use-package ispell
  :config
  (setq-default ispell-program-name "enchant-2"
                ;; Hide all default entries which may not be available
                ;; on the system anyways
                ispell-dictionary-base-alist nil
                ispell-local-dictionary "en"))

(use-package flyspell
  :commands flyspell-mode
  :init
  :hook ((prog-mode . flyspell-mode)
         (text-mode . flyspell-mode)
         (conf-mode . flyspell-mode))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (custom-set-faces
   ;; TODO: for now use same style for duplicate and incorrect
   `(flyspell-duplicate ((t (:underline (:style wave :color ,(modus-themes-get-color-value 'underline-note))))))
   '(flyspell-incorrect ((t (:inherit modus-themes-lang-note))))))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c s p" . flyspell-correct-previous)
              ("C-c s n" . flyspell-correct-next)
              ("C-c s c" . flyspell-correct-at-point)
              ("C-c s d" . ispell-change-dictionary)
              ("C-c s f" . flyspell-buffer))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-dummy))

(use-package languagetool
  :commands (languagetool-server-mode
             languagetool-server-start)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")))

;;;; * Project
(use-package project
  :bind (:map project-prefix-map
              ("f" . project-find-file)
              ("d" . project-find-dir)
              ("p" . project-switch-project)
              ("r" . project-query-replace-regexp)
              ("g" . magit-project-status)
              ("s" . consult-ripgrep)
              ("?" . flymake-show-project-diagnostics))
  :config
  (setq project-switch-commands
        '((project-find-file "file")
          (consult-ripgrep "search")
          (project-find-dir "directory")
          (project-dired "browse")
          (magit-project-status "vc")
          (project-eshell "shell")))
  :init
  (defun jmpunkt/project-smylink-in-dir? (file)
    "Check if a given file is inside the project directory.

If the file is a symlink, it must point to a valid location inside the
project. If that link points into the project root, we keep it, otherwise we
ignore it. If the file is not a symlink, then it is assumed it is already inside
the project root. "
    (let ((project (project-current))
          (symlink-resolved (file-symlink-p file)))
      (if (and symlink-resolved (file-exists-p symlink-resolved))
          (if project
              (file-in-directory-p symlink-resolved (project-root project))
            t)
        t)))
  (define-advice project-query-replace-regexp
      (:around (orig from to) symlink-filter)
    ;; remember old project-files
    (let ((orig-project-files (symbol-function 'project-files)))
      ;; overwrite project-files with symlink filtering
      (cl-letf (((symbol-function 'project-files)
                 (lambda (project)
                   (seq-filter #'jmpunkt/project-smylink-in-dir? (funcall orig-project-files project)))))
        (funcall orig from to))))
  ;; ignore all projects inside the nix store
  (define-advice project-try-vc
      (:around (orig dir) nix-store-filter)
    (let ((orig-locate (symbol-function 'locate-dominating-file)))
      (cl-letf (((symbol-function 'locate-dominating-file)
                 (lambda (file name)
                   (if (s-starts-with? "/nix/store/" file)
                       nil
                     (funcall orig-locate file name)))))
        (funcall orig dir))))
  (define-advice project-search
      (:around (orig from to) symlink-filter)
    ;; remember old project-files
    (let ((orig-project-files (symbol-function 'project-files)))
      ;; overwrite project-files with symlink filtering
      (cl-letf (((symbol-function 'project-files)
                 (lambda (project)
                   (seq-filter #'jmpunkt/project-smylink-in-dir? (funcall orig-project-files project)))))
        (funcall orig from to))))
  (defun jmpunkt/nix-read-template (flake-ref)
    "Selects one of the provided Nix templates by FLAKE-REF."
    (completing-read "Template: " (mapcar #'car (nix--process-json "eval" (format-message "%s#templates" flake-ref) "--json"))))
  (defun jmpunkt/nix-read-registry ()
    "Selects one of the available Nix registries."
    (completing-read "Registry: " (mapcar #'cadr (nix-flake--registry-list))))
  (defun jmpunkt/project-new ()
    "Creates a new Nix project with a interactively selected template."
    (interactive)
    (require 'nix-flake)
    ;; NOTICE: Use specific registry instead of querying all
    ;; registries for templates. Querying all registries would require
    ;; to download them all, which would slow down the creation
    ;; process. The optimal way would be to bundle all used templates
    ;; under one registry.
    (let* ((flake-ref (jmpunkt/nix-read-registry))
           (template-name (jmpunkt/nix-read-template flake-ref))
           (dir (read-directory-name "Target directory: "))
           (name (read-string (format-message "Project name [creating new folder in %s]: " dir))))
      (with-temp-buffer
        (cd dir)
        (make-directory name)
        (cd name)
        (nix-flake--init flake-ref template-name))))
  (defun jmpunkt/project-init ()
    ""
    (interactive)
    (require 'nix-flake)
    ;; NOTICE: Use specific registry instead of querying all
    ;; registries for templates. Querying all registries would require
    ;; to download them all, which would slow down the creation
    ;; process. The optimal way would be to bundle all used templates
    ;; under one registry.
    (let* ((flake-ref (jmpunkt/nix-read-registry))
           (template-name (jmpunkt/nix-read-template flake-ref))
           (dir (project-root (project-current t))))
      (with-temp-buffer
        (cd dir)
        (nix-flake--init flake-ref template-name))))
  (defun jmpunkt/project-compile-setup (proc)
    "Adds the project root directory to the `compilation-search-path'.

Added at the end of the list (`compilation-search-path'). If the
compilation-mode can not jump to the reference with the previous
paths, it will fallback to the project root path."
    (let ((root (project-root (project-current nil))))
      (when root
        (add-to-list 'compilation-search-path root 1))))
  (add-hook 'compilation-start-hook #'jmpunkt/project-compile-setup))

;;;; * Language Server (LSP)
(use-package eglot
  :commands eglot-ensure
  :bind (:map eglot-mode-map
              ("C-c k r" . jmpunkt/eglot-rename)
              ("C-c k h" . jmpunkt/eglot-code-actions))
  :init
  (defun jmpunkt/lsp-snippet-to-tempel (snippet)
    "Convert Language Server Protocol snippet into Tempel snippet."
    (let ((regex (rx (or
                      (and (group-n 10 "$") (group (+ digit)))
                      (and (group-n 10 "${") (group (+ digit)) ":" (+? anychar) "}")))))
      (save-match-data
        (setq last-match 0
              template '())
        (while (string-match regex snippet last-match)
          (setq template (append template `(,(substring snippet last-match (or (when (match-beginning 1) (1- (match-beginning 1))) (match-beginning 10)))
                                            p)))
          (setq last-match (match-end 0)))
        (add-to-list 'template (substring snippet last-match (length snippet)) 1)
        template)))
  (defun jmpunkt/eglot-rename ()
    (interactive)
    (meow--cancel-selection)
    (call-interactively 'eglot-rename))
  (defun jmpunkt/eglot-code-actions ()
    (interactive)
    (meow--cancel-selection)
    (call-interactively 'eglot-code-actions))
  (define-advice eglot--snippet-expansion-fn
      (:override (&rest args) tempel-advice)
    (require 'tempel)
    (lambda (snippet) (tempel-insert (jmpunkt/lsp-snippet-to-tempel snippet))))
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'eldoc-highlight-function-argument)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio"))
  (setq eglot-extend-to-xref t
        eglot-stay-out-of '(company)
        eglot-events-buffer-config '(:size 0 :format lisp)
        eglot-sync-connect 0
        eglot-confirm-server-initiated-edits nil))

(use-package eglot-x
  :demand t
  :after eglot)

;;;; * Flymake
(use-package flymake
  :after consult
  :commands flymake-mode
  :bind (:map flymake-mode-map
              ("C-c h f" . consult-flymake)
              ("C-c h d" . flymake-show-buffer-diagnostics))
  :hook ((prog-mode . flymake-mode)
         (text-mode . flymake-mode)
         (flymake-diagnostics-buffer-mode . visual-line-mode)
         (flymake-project-diagnostics-mode . visual-line-mode))
  :config
  (setq flymake-mode-line-counter-format
        '(" " flymake-mode-line-error-counter
          " " flymake-mode-line-warning-counter
          " " flymake-mode-line-note-counter
          " ")))

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
  :config
  (setq shr-use-colors nil
        shr-bullet "• "
        shr-folding-mode t))

(use-package browse-url
  :config
  (setq browse-url-handlers '((".*youtube.*" . browse-url-firefox)
                              (".*raw\.githubusercontent.*" . browse-url-emacs)
                              (".*github.*" . browse-url-firefox)
                              ("." . eww-browse-url))))
(use-package eww
  :commands (eww eww-follow-link)
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html?q="))

;;;; Search/Find
(use-package minibuffer
  :hook ((minibuffer-setup . cursor-intangible-mode)
         (minibuffer-setup . minibuffer-depth-indicate-mode)
         (minibuffer-setup . (lambda ()
                               (setq-local minibuffer-local-shell-command-map (make-sparse-keymap)))))
  :bind (:map minibuffer-local-map
              ([remap keyboard-quit] . minibuffer-keyboard-quit)
              ("<escape>" . minibuffer-exit)
              ("<return>" . exit-minibuffer)
              ("\t" . minibuffer-force-complete)
              ("C-j" . next-line)
              ("C-k" . previous-line)
              ("M-j" . scroll-up-command)
              ("M-k" . scroll-down-command)
              ("M-c" . kill-whole-line)
              ("C-b" . beginning-of-buffer)
              ("C-e" . end-of-buffer)
              ("M-n" . next-history-element)
              ("M-p" . previous-history-element))
  :config
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        enable-recursive-minibuffers t
        completion-ignore-case t))

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

(use-package vertico
  :init
  (require 'vertico-quick)
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
              ("M-q" . vertico-quick-insert)
              ("C-q" . vertico-quick-exit))
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode)
  :config
  (setq prescient-filter-method '(literal regexp initialism)))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind (:map global-map
              ("C-x b" . consult-buffer)
              ("C-x C-b" . consult-buffer-other-window)
              ("M-o" . consult-outline)
              ("C-s" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (defun consult-clock-in ()
    "Clock into an Org heading."
    (interactive)
    (save-window-excursion
      (consult-org-heading)
      (org-clock-in)))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize consult-clock-in
                     :prompt "Clock in: "
                     :preview-key "M-.")
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "C-."))

(use-package corfu-prescient
  :commands corfu-prescient-mode)

(use-package corfu
  :commands corfu-mode
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :hook (corfu-mode . corfu-prescient-mode))

(use-package kind-icon
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15))

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  (defalias 'cape-symbol+dabbrev
    (cape-capf-super #'cape-elisp-symbol #'cape-dabbrev))
  (defun jmpunkt/cape-setup-git-commit ()
    (setq completion-at-point-functions '(cape-dabbrev)))
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

(use-package rg
  :commands (rg-menu)
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu))

;;;; * breadcrumb
(use-package breadcrumb
  :hook (after-init . breadcrumb-mode))

;;;; * VCS
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options "-w"))

(use-package magit
  :bind ((:map global-map
               ("C-x g" . magit-status)
               ("C-x G" . magit-status-here))
         (:map magit-status-mode-map
               ("t" . magit-discard)
               ("T" . magit-tag)
               ("C-t" . magit-notes)
               ("L" . magit-log)
               ("C-l" . magit-log-refresh)))
  :config
  ;; NOTE: The order is important, first rebind keybinds which should
  ;;       be used late. Otherwise, the menu will miss these entries
  ;;       and the replace operations fails silently.
  (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
  '("x" "Absorb changes" magit-commit-absorb))
  (transient-replace-suffix 'magit-dispatch
    '("T" "Note" magit-notes)
    '("C-t" "Note" magit-notes))
  (transient-replace-suffix 'magit-dispatch
    '("t" "Tag" magit-tag)
    '("T" "Tag" magit-tag))
  (transient-replace-suffix 'magit-dispatch
    '("k" "Discard" magit-discard)
    '("t" "Discard" magit-discard))
  (transient-replace-suffix 'magit-dispatch
    '("L" "Log (change)" magit-log-refresh)
    '("C-l" "Log (change)" magit-log-refresh))
  (transient-replace-suffix 'magit-dispatch
    '("l" "Log" magit-log)
    '("L" "Log" magit-log))
  (setq magit-slow-confirm '(magit-discard))
  (setq magit-diff-refine-hunk 'all))

(use-package git-commit
  :config
  (setq git-commit-style-convention-checks
        '(overlong-summary-line non-empty-second-line)))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode)
         (after-init . global-diff-hl-mode)))

;;;; * Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . (lambda () (setq-local tab-width 8)))
  :init
  (defun jmpunkt/format-org-src ()
    "Formats the org-src-block with C-c C-f keybind or indents the whole buffer.

If the cursor is inside a source block, then the C-c C-f keybind is
delegated to the underlying mode. C-c C-f is ideally bound to
formatting the source code. If the cursor is not inside a source
block, then the whole buffer is indented."
    (interactive)
    (if (org-in-src-block-p)
        (progn
          (org-edit-special)
          (let ((formatter (key-binding (kbd "C-c C-f"))))
            (when formatter
              (funcall formatter)))
          (org-edit-src-exit))
      (org-indent-indent-buffer)))
  :bind (:map org-mode-map
              ("C-c C-f" . jmpunkt/format-org-src)
              ("C-j" . nil)
              ("C-c <return>" . org-edit-src-exit))
  :hook ((org-mode . (lambda ()
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
  (setq org-ellipsis "…"
        org-log-done 'time
        org-catch-invisible-edits 'show-and-error
        org-latex-pdf-process '("tectonic -X compile --outdir=%o -Z shell-escape -Z continue-on-errors %f")))

(use-package org-clock
  ;; Same as (org-clock-persistence-insinuate) but will lazy load. Calling the
  ;; function instead causes `org-mode' to be loaded.
  :hook ((org-mode . org-clock-load)
         (kill-emacs . org-clock-save))
  :config
  (setq org-clock-idle-time 10
        org-clock-in-resume t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-out-remove-zero-time-clocks t))

(use-package ox-typst
  :demand t
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("typst" . typst-ts))
  (add-to-list 'org-latex-listings-langs '(typst-ts "Typst")))

(use-package ob-shell
  :after org
  :commands (org-babel-execute:shell))

(use-package ob-graphql
  :after org
  :commands (org-babel-execute:graphql))

(use-package ob-sql
  :after org
  :commands (org-babel-execute:sql))

(use-package ob-sqlite
  :after org
  :commands (org-babel-execute:sqlite))

(use-package ob-python
  :after org
  :commands (org-babel-execute:python))

(use-package ob-latex
  :after org
  :commands (org-babel-execute:latex
             org-babel-expand-body:latex))

(use-package ob-gnuplot
  :after org
  :commands (org-babel-expand-body:gnuplot))

(use-package ob-dot
  :after org
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package ob-emacs-lisp
  :after org
  :commands (org-babel-execute:emacs-lisp
             org-babel-expand-body:emacs-lisp))

(use-package ob-plantuml
  :after org
  :commands (org-babel-execute:plantuml))

(use-package ob-async
  :after org
  :commands (ob-async-org-babel-execute-src-block
             org-babel-execute-src-block:async))

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
  :after org
  :hook (org-mode . org-indent-mode))

(use-package org-src
  :demand t
  :after org
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t))

(use-package citar
  :bind ("C-c b" . citar-insert-citation))

;;; * Configuration Files
;;;; * Mermaid
(use-package mermaid-mode)

;;;; * YAML
(use-package yaml-ts-mode
  :hook (yaml-ts-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode)))

;;;; * TOML
(use-package toml-ts-mode)

;;;; * GraphQL
(use-package graphql-mode
  :fmt (graphql-mode . fmt/prettier-buffer)
  :hook (graphql-mode . fmt/prettier-on-save-mode))

;;; * Text Files
;;;; * reStructuredText
(use-package rst
  :mode (("\\.txt\\'" . rst-mode)
         ("\\.rst\\'" . rst-mode)
         ("\\.rest\\'" . rst-mode)))

;;;; * Markdown
(use-package markdown-mode
  :hook ((markdown-mode . flyspell-mode))
  :mode
  (("INSTALL\\.md\\\'". gfm-mode)
   ("CONTRIBUTORS\\.md\\\'". gfm-mode)
   ("LICENSE\\.md\\\'". gfm-mode)
   ("README\\.md\\\'". gfm-mode)))

;;;; * Graphivz
(use-package graphviz-dot-mode)

;;;; * Makefile
(use-package make-mode
  :hook (make-mode . indent-tabs-mode))

;;; * Programming Languages

;;;; * SQL
(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :fmt (sql-mode . fmt/pgformatter-buffer)
  :config
  (sql-highlight-postgres-keywords))

;;;; * Typst
(use-package typst-ts-mode
  :mode "\\.typ\\'"
  :hook ((typst-ts-mode . fmt-mode)
         ;; (typst-ts-mode . eglot-ensure)
         (typst-ts-mode . fmt/typstyle-on-save-mode))
  :fmt (typst-ts-mode . fmt/typstyle-buffer)
  :custom
  (typst-ts-mode-indent-offset 2)
  :config
  (require 'eglot)
  (add-to-list 'eglot-server-programs '((typst-ts-mode) . ("typst-lsp"))))

;;;; * Nix
(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook ((nix-ts-mode . eglot-ensure)
         (nix-ts-mode . fmt/alejandra-on-save-mode))
  :fmt (nix-ts-mode . fmt/alejandra-buffer)
  :config
  (require 'eglot)
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd"))))

;;;; * C/C++
(use-package c-ts-mode
  :init
   (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
   (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
   (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

;;;; * Python
(use-package python
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :hook (python-base-mode . eglot-ensure)
  :fmt (python-mode . fmt/ruff-buffer)
  :config
  (setq python-indent-offset 4)
  (push 'pyright compilation-error-regexp-alist)
  (push '(pyright "^\\ +\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).+$" 1 2 3) compilation-error-regexp-alist-alist))

(use-package flymake-ruff
  :hook (python-ts-mode . flymake-ruff-load))

;;;; * Rust
(use-package rust-ts-mode
  :hook (rust-ts-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

  :config
  (add-to-list 'treesit-thing-settings
               '(rust (defun (or "function_item" "closure_expression"))
                      ;; (sexp "")
                      ;; (word "")
                      ;; (sentence "")
                      (paragraph "block")
                      (string (or "string_literal" "raw_string_literal"))
                      (text (or "line_comment" "string_literal" "raw_string_literal"))
                      (comment "line_comment")
                      (number (or "integer_literal" "float_literal")))))

(use-package rust-compile
  :demand t
  :after rust-ts-mode)

;;;; * Lua
(use-package lua-ts-mode
  :mode (("\\.lua\\'" . lua-ts-mode)))

;;;; * WEB
(use-package sgml-mode
  :hook ((html-mode . eglot-ensure)
         (html-mode . fmt/prettier-on-save-mode))
  :fmt (html-mode . fmt/prettier-buffer))

(use-package css-mode
  :init
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  :mode (("\\.scss\\'" . scss-mode))
  :hook ((css-base-mode . eglot-ensure)
         (css-base-mode . fmt/prettier-on-save-mode)
         (scss-base-mode . eglot-ensure)
         (scss-mode . fmt/prettier-on-save-mode))
  :fmt (css-base-mode . fmt/prettier-buffer)
  :config
  (setq css-indent-offset 2))

(use-package typescript-ts-mode
  :hook ((typescript-ts-base-mode . eglot-ensure)
         (typescript-ts-base-mode . fmt/biome-buffer))
  :fmt (typescript-ts-base-mode . fmt/biome-buffer))

(use-package json-ts-mode
  :mode (("flake.lock\\'" . json-ts-mode))
  :fmt (json-ts-mode . fmt/biome-buffer))

(use-package js
  :hook ((js-base-mode . eglot-ensure)
         (js-base-mode . fmt/biome-buffer))
  :fmt (js-base-mode . fmt/biome-buffer)
  :config
  (setq js-indent-level 2))

;;;; * LaTeX
(use-package tex-mode
  :mode ("\\.tex\\'" . latex-mode))

;;;; * Docker
(use-package dockerfile-ts-mode)

(use-package docker
  :bind ("C-c C-d" . docker))

;;; * PDF
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (pdf-misc-size-indication-minor-mode)
                           (pdf-links-minor-mode)
                           (pdf-isearch-minor-mode)
                           (auto-revert-mode)
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

;;; * Misc
(use-package p-search
  :commands p-search)

(use-package devdocs
  :commands devdocs-lookup
  :bind (:map global-map ("C-h D" . devdocs-lookup)))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-mode-map
              ("C-c r" . copilot-accept-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))

(use-package biome
  :commands biome
  :config
  (setq biome-query-coords
      '(("Berlin, Germany" 52.52437 13.41053)
        ("Darmstadt, Germany" 49.87167 8.65027))))

(use-package proced
  :bind (:map proced-mode-map
              ("C-s" . jmpunkt/proced-search))
  :init
  (defun jmpunkt/proced-search ()
    "Search inside the proced buffer.

Disabling the auto update while searching."
    (interactive)
    (let ((save-status proced-auto-update-flag))
      (unwind-protect
          (progn
            (setq-local proced-auto-update-flag nil)
            (call-interactively #'consult-line))
        (setq-local proced-auto-update-flag save-status))))
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-tree-flag t
        proced-auto-update-interval 1
        proced-auto-update-flag 'visible
        proced-show-remote-processes t
        proced-enable-color-flag t))

;;; * -- End
(provide 'init)
;;; init.el ends here

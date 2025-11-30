;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
;;; Startup Improvements

;; Prevent Emacs from uncompromising loaddef files and skipping them
;; anyways. For this setup (NixOS) every package should be compiled anyways.
(setq native-comp-jit-compilation-deny-list '(".*-loaddefs.el.gz"))

;; persistent settings
(setq read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering t
      gc-cons-threshold (* 128 1024 1024))

;;; Packages
(setq use-package-always-defer t)
(setq package-archives nil)
(setq use-package-verbose nil)
(setq use-package-compute-statistics t)

;;; Emacs
(setq default-frame-alist '((font . "Fantasque Sans Mono")))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; https://github.com/jamescherti/minimal-emacs.d/blob/b1d47f949735c186ea77e1948a3cbac7412d186a/early-init.el#L228C1-L229C64
;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; https://github.com/jamescherti/minimal-emacs.d/blob/b1d47f949735c186ea77e1948a3cbac7412d186a/early-init.el#L262
(when (and (not (daemonp)) (not noninteractive))
  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;;; * -- End
;;; early-init.el ends here

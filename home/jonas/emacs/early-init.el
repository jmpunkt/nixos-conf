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
(setq package-enable-at-startup nil)
(setq use-package-verbose nil)
(setq use-package-compute-statistics t)
(package-initialize)

;;; Emacs
(setq default-frame-alist '((font . "Fantasque Sans Mono")))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;; * -- End
;;; early-init.el ends here

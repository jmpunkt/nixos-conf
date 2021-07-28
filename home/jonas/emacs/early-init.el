;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
;;; Startup Improvements
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;;; Packages
(setq package-archives nil)
(setq package-enable-at-startup nil)
(setq use-package-verbose t)
(package-initialize)

;;; Emacs
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; * -- End
;;; early-init.el ends here

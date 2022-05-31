;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
;;; Startup Improvements
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering t)

;;; Packages
(setq package-archives nil)
(setq package-enable-at-startup nil)
(setq use-package-verbose t)
(package-initialize)

;;; Emacs
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(with-eval-after-load "tsc-dyn-get"
  ;; Due to NixOS, tsc-dyn should always be present at the most recent
  ;; version.
  (advice-add 'tsc-dyn-get-ensure :override
              (lambda (version)
                (let ((load-path (nconc `(,tsc-dyn-dir) load-path)))
                  (message "using advide for tsc")
                  (require 'tsc-dyn)))))

;;; * -- End
;;; early-init.el ends here

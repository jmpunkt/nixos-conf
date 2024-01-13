;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;;; Packages
;;; Startup Improvements
;; temporary settings
;; persistent  settings
(setq read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering t
      gc-cons-threshold (* 128 1024 1024))

;;; Packages
(defun jmpunkt/use-package-statistics-convert (package)
  "Return information about PACKAGE.

The information is formatted in a way suitable for
`use-package-statistics-mode'."
  (let ((statistics (gethash package use-package-statistics)))
    (list
     package
     (vector
      (symbol-name package)
      (use-package-statistics-status statistics)
      (format-time-string
       "%H:%M:%S.%6N"
       (use-package-statistics-last-event statistics))
      (format "%.4f" (use-package-statistics-time statistics))))))

(advice-add 'use-package-statistics-convert :override #'jmpunkt/use-package-statistics-convert)

(setq package-archives nil)
(setq package-enable-at-startup nil)
(setq use-package-verbose nil)
(setq use-package-compute-statistics t)
(package-initialize)

;;; Emacs
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; * -- End
;;; early-init.el ends here

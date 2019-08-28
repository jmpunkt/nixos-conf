;;; init.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

(load-file "~/.emacs.d/core.el")
(load-file "~/.emacs.d/globals.el")
(load-file "~/.emacs.d/init-hydra.el")

(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/config")
(load-directory "~/.emacs.d/lang")
(load-directory "~/.emacs.d/misc")

(provide 'init)
;;; init.el ends here

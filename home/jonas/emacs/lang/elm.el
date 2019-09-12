;;; elm.el --- Configuration for Elm developement environment
;;; Commentary:

;;; Code:
(use-package elm-mode
  :after company
  :config
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))

(provide 'elm)
;;; elm.el ends here

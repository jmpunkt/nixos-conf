;;; init-git.el --- Initialization git support
;;; Commentary:

;;; Code:
(use-package magit
  :config
  ;; Ignore recent commit
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
          magit-insert-unpushed-to-pushremote)
        ;; Update visualization
        pretty-magit-alist nil
        pretty-magit-prompt nil)

  (defhydra hydra-magit (:color blue :hint nil)
    "
  ^Magit^             ^Do^
-----------------------------------------------
  [_q_] quit          [_b_] blame
  ^^                  [_c_] clone
  ^^                  [_i_] init
  ^^                  [_s_] status
  ^^                  ^^
"
    ("q" nil)
    ("b" magit-blame)
    ("c" magit-clone)
    ("i" magit-init)
    ("s" magit-status))

  ;; Opening repo externally
  (defun parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))
  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (parse-url url))
        (message "opening repo %s" url))))

  (add-hook 'magit-mode-hook
            (lambda ()
              (local-set-key (kbd "o") 'magit-open-repo))))

(use-package evil-magit)

(provide 'init-git)
;;; init-git.el ends here

;;; git.el --- Initialization file for Emacs
;;; Commentary:

;;; Code:
(use-package magit
  :ensure t
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
          magit-insert-unpushed-to-pushremote))


  ;; Update visualization
  (setq pretty-magit-alist nil
        pretty-magit-prompt nil)

  (defmacro pretty-magit (WORD ICON PROPS &optional NO-PROMPT?)
    "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
    `(prog1
         (add-to-list 'pretty-magit-alist
                      (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                            ,ICON ',PROPS))
       (unless ,NO-PROMPT?
         (add-to-list 'pretty-magit-prompt (concat ,WORD ": ")))))

  ;; Operations
  (pretty-magit "add"   ? (:foreground "#375E97" :height 1.2) pretty-magit-prompt)
  (pretty-magit "fix"   ? (:foreground "#FB6542" :height 1.2) pretty-magit-prompt)
  (pretty-magit "clean" ? (:foreground "#FFBB00" :height 1.2) pretty-magit-prompt)
  (pretty-magit "doc."  ? (:foreground "#3F681C" :height 1.2) pretty-magit-prompt)

  ;; Meta information
  (pretty-magit "master"  ? (:box nil :height 1.2) t)
  (pretty-magit "origin"  ? (:box nil :height 1.2) t)
  (pretty-magit "upstream"  ? (:box nil :height 1.2) t)

  (defun add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (--each pretty-magit-alist
        (-let (((rgx icon props) it))
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (compose-region
               (match-beginning 1) (match-end 1) icon)
              (when props
                (add-face-text-property
                 (match-beginning 1) (match-end 1) props))))))))

  (advice-add 'magit-status :after 'add-magit-faces)
  (advice-add 'magit-refresh-buffer :after 'add-magit-faces)


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

(require 'evil-magit)
              
(provide 'git)
;;; git.el ends here

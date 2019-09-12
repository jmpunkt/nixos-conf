;;; init-paper.el --- Configuration for papers
;;; Commentary:

;;; Code:

(use-package academic-phrases)

(use-package langtool
  :init
  ;; for NixOS use languagetool-commandline?
  (setq langtool-bin "languagetool-commandline"))

(use-package org
  :after flyspell
  :init
  (setq papers-dir (expand-file-name "~/Documents/papers/")
        papers-pdfs (concat papers-dir "lib/")
        papers-notes (concat papers-dir "index.org")
        papers-refs (concat papers-dir "index.bib")
        org-highlight-latex-and-related '(latex)
        org-bullets-bullet-list '("●" "○" "✸" "✿")
        org-ellipsis "…"
        org-catch-invisible-edits 'smart)
  :hook ((org-mode . flyspell-mode))
  :config
  (setq org-agenda-files '("~/Documents/notes/"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (js . t)
     (python . t)
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (gnuplot . t)
     (sql . t)
     (dot . t))))

(use-package org-ref
  :after org
  :config
  ;; (setq reftex-default-bibliography (list papers-refs))
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes papers-notes
        org-ref-default-bibliography (list papers-refs)
        org-ref-pdf-directory papers-pdfs))

(use-package org-noter
  :after org
  :commands org-noter
  :config
  (setq org-noter-default-notes-file-names '("index-org")
	  org-noter-notes-search-path (list papers-dir)
	  org-noter-auto-save-last-location t
	  org-noter-doc-split-fraction '(0.8 . 0.8)
	  org-noter-always-create-frame nil
	  org-noter-insert-note-no-questions t
	  org-noter-notes-window-location 'vertical-split))

(use-package ivy-bibtex
  :after (ivy org)
  :config
  (setq bibtex-completion-bibliography papers-refs
        bibtex-completion-library-path papers-pdfs
        bibtex-completion-notes-path papers-notes))


(use-package interleave
  :after org
  :bind (("C-x i" . interleave-mode))
  :config
  (setq interleave-split-direction 'horizontal
        interleave-split-lines 20
        interleave-disable-narrowing t))


(provide 'init-paper)
;;; init-paper.el ends here

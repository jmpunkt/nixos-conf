;;; latex.el --- Configuration for Latex environment
;;; Commentary:

;;; Code:

(use-package tex-site
  :ensure auctex
  :defer t
  :after (tex latex)
  :config

  ;; Spelling
  (setq ispell-tex-skip-alists
        '((
           ;;("%\\[" . "%\\]") ; AMStex block comment...
           ;; All the standard LaTeX keywords from L. Lamport's guide:
           ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly
           ;; \input, \label, \nocite, \rule (in ispell - rest included here)
           ("\\\\addcontentsline"              ispell-tex-arg-end 2)
           ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
           ("\\\\\\([aA]lph\\|arabic\\)"   ispell-tex-arg-end)
           ("\\\\author"                         ispell-tex-arg-end)
           ;; New regexps here --- kjh
           ("\\\\\\(text\\|paren\\)cite" ispell-tex-arg-end)
           ("\\\\cite\\(t\\|p\\|year\\|yearpar\\)" ispell-tex-arg-end)
           ("\\\\bibliographystyle"                ispell-tex-arg-end)
           ("\\\\makebox"                  ispell-tex-arg-end 0)
           ("\\\\e?psfig"                  ispell-tex-arg-end)
           ("\\\\document\\(class\\|style\\)" .
            "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
          (
           ;; delimited with \begin.  In ispell: displaymath, eqnarray,
           ;; eqnarray*, equation, minipage, picture, tabular,
           ;; tabular* (ispell)
           ("\\(figure\\|table\\)\\*?"     ispell-tex-arg-end 0)
           ("\\(equation\\|eqnarray\\)\\*?"     ispell-tex-arg-end 0)
           ("list"                                 ispell-tex-arg-end 2)
           ("program" . "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
           ("verbatim\\*?"."\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")
           ("lstlisting\\*?"."\\\\end[ \t\n]*{[ \t\n]*lstlisting\\*?[ \t\n]*}"))))

  ;; Pdf activated by default
  (TeX-global-PDF-mode 1)

  ;; Diverse
  (setq-default TeX-master nil)
  (setq TeX-parse-self t
        TeX-auto-save t)

  ;; Filling
  (add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode t)))

  ;; Minor helpers for comment and quotes
  (add-to-list 'LaTeX-verbatim-environments "comment")
  (setq TeX-open-quote "\enquote{"
        TeX-close-quote "}")

  ;; Indentation
  (setq LaTeX-indent-level 4
        LaTeX-item-indent 0
        TeX-brace-indent-level 4
        TeX-newline-function 'newline-and-indent)


  ;; Some usefull hooks
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)

  (defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
    "Start each sentence on a new line."
    (let ((from (ad-get-arg 0))
          (to-marker (set-marker (make-marker) (ad-get-arg 1)))
          tmp-end)
      (while (< from (marker-position to-marker))
        (forward-sentence)
        ;; might have gone beyond to-marker --- use whichever is smaller:
        (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
        ad-do-it
        (ad-set-arg 0 (setq from (point)))
        (unless (or
                 (bolp)
                 (looking-at "\\s *$"))
          (LaTeX-newline)))
      (set-marker to-marker nil)))
  (ad-activate 'LaTeX-fill-region-as-paragraph)



  ;; PDF/Tex correlation
  (setq TeX-source-correlate-method 'synctex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  ;; Keys
  (define-key LaTeX-mode-map (kbd "C-c C-=") 'align-current))

(use-package bibtex
  :defer t
  :config
  (defun bibtex-generate-autokey ()
    (let* ((bibtex-autokey-names nil)
           (bibtex-autokey-year-length 2)
           (bibtex-autokey-name-separator "\0")
           (names (split-string (bibtex-autokey-get-names) "\0"))
           (year (bibtex-autokey-get-year))
           (name-char (cond ((= (length names) 1) 4)
                            ((= (length names) 2) 2)
                            (t 1)))
           (existing-keys (bibtex-parse-keys))
           key)
      (setq names (mapconcat (lambda (x)
                               (substring x 0 name-char))
                             names
                             ""))
      (setq key (format "%s%s" names year))
      (let ((ret key))
        (loop for c from ?a to ?z
              while (assoc ret existing-keys)
              do (setq ret (format "%s%c" key c)))
        ret)))

  (setq bibtex-align-at-equal-sign t
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-first-ignore '("the" "a" "if" "and" "an")
        bibtex-autokey-titleword-length 100
        bibtex-autokey-titlewords 1))


;; (setq TeX-auto-global (format "%s/auctex/style" generated-basedir))

(use-package company-auctex
  :ensure t
  :hook
  (latex-mode . (company-auctex-init)))


(use-package company-bibtex
  :ensure t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex)))))

(use-package company-reftex
  :ensure t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations)))))

(use-package company-math
  :ensure t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode)))))
;; Escape mode
(defun TeX-toggle-escape nil
  (interactive)
  "Toggle Shell Escape"
  (setq LaTeX-command
        (if (string= LaTeX-command "latex")
            "latex -shell-escape"
          "latex"))
  (message (concat "shell escape "
                   (if (string= LaTeX-command "latex -shell-escape")
                       "enabled"
                     "disabled"))
           )
  )

(use-package auctex-latexmk
  :ensure t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(setq TeX-show-compilation nil)

;; Redine TeX-output-mode to get the color !
(define-derived-mode TeX-output-mode TeX-special-mode "LaTeX Output"
  "Major mode for viewing TeX output.
  \\{TeX-output-mode-map} "
  :syntax-table nil
  (set (make-local-variable 'revert-buffer-function)
       #'TeX-output-revert-buffer)

  (set (make-local-variable 'font-lock-defaults)
       '((("^!.*" . font-lock-warning-face) ; LaTeX error
          ("^-+$" . font-lock-builtin-face) ; latexmk divider
          ("^\\(?:Overfull\\|Underfull\\|Tight\\|Loose\\).*" . font-lock-builtin-face)
          ;; .....
          )))

  ;; special-mode makes it read-only which prevents input from TeX.
  (setq buffer-read-only nil))

(use-package reftex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  :config
  (setq reftex-save-parse-info t
        reftex-enable-partial-scans t
        reftex-use-multiple-selection-buffers t
        reftex-plug-into-AUCTeX t
        reftex-vref-is-default t
        reftex-cite-format
        '((?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?x . "[]{%l}")
          (?X . "{%l}"))

        font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{")
          ("citetitle" "[{")
          ("citetitles" "[{")
          ("headlessfullcite" "[{"))

        reftex-cite-prompt-optional-args nil
        reftex-cite-cleanup-optional-args t))

(use-package latex-math-preview
  :ensure t
  :config
  (autoload 'LaTeX-preview-setup "preview")
  (setq preview-scale-function 1.2)
  (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))


(provide 'latex)
;;; latex.el ends here

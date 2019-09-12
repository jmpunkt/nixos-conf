;;; init-pdf.el --- Configuration for PDF files
;;; Commentary:

;;; Code:

(use-package pdf-tools
  :after hydra
  :hook
  (pdf-view-mode . (lambda ()
                     (pdf-misc-size-indication-minor-mode)
                     (pdf-links-minor-mode)
                     (pdf-isearch-minor-mode)
                     (cua-mode 0)))
  :bind
  ((:map pdf-view-mode-map
         ("/" . hydra-pdftools/body)
         ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
         ("g"  . pdf-view-first-page)
         ("G"  . pdf-view-last-page)
         ("l"  . image-forward-hscroll)
         ("h"  . image-backward-hscroll)
         ("j"  . pdf-view-next-page)
         ("k"  . pdf-view-previous-page)
         ("e"  . pdf-view-goto-page)
         ("u"  . pdf-view-revert-buffer)
         ("al" . pdf-annot-list-annotations)
         ("ad" . pdf-annot-delete)
         ("aa" . pdf-annot-attachment-dired)
         ("am" . pdf-annot-add-markup-annotation)
         ("at" . pdf-annot-add-text-annotation)
         ("y"  . pdf-view-kill-ring-save)
         ("i"  . pdf-misc-display-metadata)
         ("s"  . pdf-occur)
         ("b"  . pdf-view-set-slice-from-bounding-box)
         ("r"  . pdf-view-reset-slice)))
  :config
  (pdf-tools-install t t t)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t
        pdf-view-resize-factor 1.1)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (add-to-list 'auto-mode-alist (cons "\\.pdf$" 'pdf-view-mode))

  (defhydra hydra-pdftools (:color blue :hint nil)
    "
      PDF tools

   Move  History   Scale/Fit                  Annotations     Search/Link     Do
------------------------------------------------------------------------------------------------
     ^^_g_^^      _B_    ^↧^    _+_    ^ ^     _al_: list    _s_: search    _u_: revert buffer
     ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   _am_: markup  _o_: outline   _i_: info
     ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     _at_: text    _F_: link      _d_: dark mode
     ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   _ad_: delete  _f_: search link
_h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     _aa_: dired
     ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   _y_:  yank
     ^^_n_^^      ^ ^  _r_eset slice box
     ^^^↓^^^
     ^^_G_^^
"
          ("\\" hydra-master/body "back")
          ("<ESC>" nil "quit")
          ("al" pdf-annot-list-annotations)
          ("ad" pdf-annot-delete)
          ("aa" pdf-annot-attachment-dired)
          ("am" pdf-annot-add-markup-annotation)
          ("at" pdf-annot-add-text-annotation)
          ("y"  pdf-view-kill-ring-save)
          ("+" pdf-view-enlarge :color red)
          ("-" pdf-view-shrink :color red)
          ("0" pdf-view-scale-reset)
          ("H" pdf-view-fit-height-to-window)
          ("W" pdf-view-fit-width-to-window)
          ("P" pdf-view-fit-page-to-window)
          ("n" pdf-view-next-page-command :color red)
          ("p" pdf-view-previous-page-command :color red)
          ("d" pdf-view-dark-minor-mode)
          ("b" pdf-view-set-slice-from-bounding-box)
          ("r" pdf-view-reset-slice)
          ("g" pdf-view-first-page)
          ("G" pdf-view-last-page)
          ("e" pdf-view-goto-page)
          ("o" pdf-outline)
          ("s" pdf-occur)
          ("i" pdf-misc-display-metadata)
          ("u" pdf-view-revert-buffer)
          ("F" pdf-links-action-perfom)
          ("f" pdf-links-isearch-link)
          ("B" pdf-history-backward :color red)
          ("N" pdf-history-forward :color red)
          ("l" image-forward-hscroll :color red)
          ("h" image-backward-hscroll :color red)))

(provide 'init-pdf)
;;; init-pdf.el ends here

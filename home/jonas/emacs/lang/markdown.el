;;; markdown.el --- Configuration for Markdown environment
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :after (flyspell hydra)
  :mode
  ("INSTALL\\'"
   "CONTRIBUTORS\\'"
   "LICENSE\\'"
   "README\\'"
   "\\.markdown\\'"
   "\\.md\\'")
  :hook (markdown-mode . flyspell-mode)
  :bind (:map markdown-mode-map
              ([f6] . hydra-markdown-mode/body))
  :init
  (defhydra hydra-markdown-mode (:hint nil)
    "
   Formatting^^           Headings^^         References^^       Other
-------------------------------------------------------------------------------------
   [_s_] bold             [_h_] automatic    [_L_] link         [_m_] insert item
   [_e_] italic           [_1_] h1           [_U_] uri          [_l_] promote
   [_b_] blockquote       [_2_] h2           [_F_] footnote     [_r_] demote
   [_p_] pre-formatted    [_3_] h3           [_W_] wiki-link    [_u_] move up
   [_c_] code             [_4_] h4           [_R_] reference    [_d_] move down
    "
    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)

    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)

    ("m" markdown-insert-list-item)

    ("l" markdown-promote)
    ("r" markdown-demote)
    ("d" markdown-move-down)
    ("u" markdown-move-up)

    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)
    ("R" markdown-insert-reference-link-dwim :color blue)))

(use-package markdown-mode+
  :after markdown-mode
  :defer t)

(provide 'markdown)
;;; markdown.el ends here

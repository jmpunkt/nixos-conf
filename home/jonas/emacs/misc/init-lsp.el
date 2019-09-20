;;; init-lsp.el --- Configuration for lsp
;;; Commentary:

;;; Code:
(use-package lsp-mode
  :after hydra
  ;; :bind (:map lsp-mode-map
  ;;             ([f6] . hydra-lsp/body))
  :bind (([f6] . hydra-lsp/body))
  :init
  (defhydra hydra-lsp (:exit t :hint nil)
    "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
    ("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename)

    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))
  :config
  (setq lsp-inhibit-message nil
        lsp-print-performance nil
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-eldoc-prefer-signature-help nil
        lsp-eldoc-enable-hover nil
        lsp-eldoc-enable-signature-help nil
        lsp-highlight-symbol-at-point nil
        lsp-document-sync-method 'incremental
        lsp-enable-snipped t
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-enable nil))

(use-package company-lsp
  :commands company-lsp
  :after company
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(provide 'init-lsp)
;;; init-lsp.el ends here

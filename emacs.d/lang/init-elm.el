;;; init-elm.el --- My personal elm settings
;;; Commentary:

;;; Code:
(defun elm-repl-push-decl-or-region (prefix)
  "Evaluage the highlight region if highlighted, otherwise evaluate top level
declaration."

  (interactive "P")
  (if (and (mark) (use-region-p))
      (elm-repl-push (min (point) (mark)) (max (point) (mark)))
    (elm-repl-push-decl)))

(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :config
  (setq elm-sort-imports-on-save t
        elm-tags-on-save t)
  (evil-leader/set-key-for-mode 'elm-mode
    "'" 'elm-repl-load
    "," 'pop-tag-mark
    "." 'elm-mode-goto-tag-at-point
    "Rm" 'elm-preview-main
    "Rn" 'elm-preview-buffer
    "cb" 'elm-compile-buffer
    "cc" 'elm-compile-main
    "ic" 'elm-package-catalog
    "ii" 'elm-import
    "k" 'elm-oracle-doc-at-point
    "p" 'elm-repl-push-decl-or-region
    "rf" 'elm-repl-push-decl
    "ri" 'elm-repl-load
    "rr" 'elm-repl-push
    "t" 'elm-oracle-type-at-point
    )
  (add-hook 'elm-mode-hook 'elm-oracle-setup-completion)
  (after-load 'company
    (push "elm-stuff" projectile-globally-ignored-directories)
    (add-to-list 'company-backends 'company-elm))
  )

(use-package flycheck-elm
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup)
  )

(provide 'init-elm)
;;; init-elm.el ends here

;;; init-scheme.el --- My scheme configuration
;;; Commentary:

;;; Code:
(require 'init-lisp)
(require 'cmuscheme)
(require 'cl)

(use-package geiser
  :ensure t
  :config
  (require 'scheme)
  (setq geiser-active-implementations '(chicken))
  (setq scheme-program-name "csi -:c")

  (defun gieser-eval-last-sexp-or-region (prefix)
    "Evaluate region if highlighted, otherwise evaluate the PREFIX."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (geiser-eval-region (min (point) (mark)) (max (point) (mark)))
      (geiser-eval-last-sexp prefix)))

  (evil-leader/set-key-for-mode 'scheme-mode
    "cb" 'geiser-compile-current-buffer
    "cf" 'geiser-load-file
    "ee" 'geiser-eval-buffer
    "er" 'geiser-eval-region
    "k"  'geiser-doc-symbol-at-point
    "lb" 'geiser-load-current-buffer
    "lf" 'gieser-load-file
    "p"  'gieser-eval-last-sexp-or-region
    )

  (add-hook 'scheme-mode-hook '(lambda () (run-hooks 'my-lisp-modes-hook)))
  (evil-define-key 'insert scheme-mode-map
    (kbd ";") 'electric-lisp-comment)
  (add-hook 'geiser-doc-mode-hook 'help-mode)
  (add-hook 'geiser-debug-mode-hook 'help-mode)
  )

(provide 'init-scheme)
;;; init-scheme.el ends here

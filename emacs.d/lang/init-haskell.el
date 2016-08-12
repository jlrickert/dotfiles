;;; init-haskell.el --- My haskell mode configurations
;;; Commentary:

;;; Code:
(require 'company)
(use-package haskell-mode
  :ensure t
  :config
  (use-package company-ghc
    :ensure t
    :defer t
    :config
    (add-to-list 'company-backends 'company-ghc))

  (use-package shm
    :ensure t
    :disabled t
    :config
    (add-hook 'haskell-mode-hook 'structured-haskell-mode))

  (use-package intero
    :ensure t
    :config
    (add-hook 'haskell-mode-hook 'intero-mode))

  (use-package hindent
    :ensure t
    :config
    (add-hook 'haskell-mode-hook 'hindent-mode))

  (defun run-haskell ()
    (interactive)
    (haskell-session-change))

  ;; (haskell-stylish-on-save)
  ;; (add-hook 'haskell-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'company-backends)
  ;;                  (append '((company-capf company-dabbrev-code))
  ;;                          company-backends))))

  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (evil-leader/set-key-for-mode 'haskell-mode
    "." 'haskell-mode-jump-to-def-or-tag
    "rr" 'intero-repl-load
    "rc" 'ghc-kill-process
    "t" 'intero-type-at
    "p" 'intero-repl-load
    )

  (setq-default haskell-stylish-on-save t)
  (custom-set-variables
   '(haskell-tags-on-save t)
   '(company-ghc-show-info t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-type 'stack-ghci)
   )

  (add-hook 'haskell-mode-hook
            '(lambda ()
               (subword-mode)
               (haskell-auto-insert-module-template)
               (ghc-init)
               ))
  (add-hook 'haskell-error-mode-hook '(lambda () (help-mode)))
  )

(provide 'init-haskell)
;;; init-haskell.el ends here

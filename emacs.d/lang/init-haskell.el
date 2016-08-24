;;; init-haskell.el --- My haskell mode configurations
;;; Commentary:

;;; Code:
(require 'company)
(use-package haskell-mode
  :ensure t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook 'ghc-init)
  (setq haskell-notify-p t
        haskell-tags-on-save t
        haskell-interactive-popup-errors nil
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-stylish-on-save t
        )
  :config
  (use-package cmm-mode :ensure t :defer t)

  (use-package company-ghc
    :ensure t
    :defer t
    :config
    (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
    )

  (use-package company-cabal
    :ensure t
    :defer t
    :config
    (add-to-list 'company-backends 'company-cabal))

  (use-package flycheck-haskell
    :ensure t
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
    ;; (set-face-attribute 'ghc-face-error nil :underline nil)
    ;; (set-face-attribute 'ghc-face-warn nil :underline nil)
    )

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
    :defer t
    :init
    (add-hook 'haskell-mode-hook 'hindent-mode)
    :config
    (setq hindent-style "johan-tibell")
    )

  (defun run-haskell ()
    (interactive)
    (haskell-session-change))

  ;; (haskell-stylish-on-save)
  ;; (add-hook 'haskell-mode-hook
  ;;           (lambda ()
  ;;             (set (make-local-variable 'company-backends)
  ;;                  (append '((company-capf company-dabbrev-code))
  ;;                          company-backends))))
  (setq-default haskell-stylish-on-save t)
  (custom-set-variables
   '(haskell-tags-on-save t)
   '(company-ghc-show-info t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-type 'cabal-repl)
   )

  (evil-leader/set-key-for-mode 'haskell-mode
    "." 'haskell-mode-jump-to-def-or-tag
    "=" 'hindent-reformat-buffer
    "m<" 'ghc-make-indent-shallower
    "m>"  'ghc-make-indent-deeper
    "mn" 'ghc-goto-next-hole
    "mp" 'ghc-goto-prev-hole
    "p" 'intero-repl-load
    "rc" 'ghc-kill-process
    "rr" 'intero-repl-load
    "t" 'intero-type-at
    "k" 'hoogle
    "K" 'haskell-hoogle-lookup-from-local
    )

  (add-hook 'haskell-mode-hook
            '(lambda ()
               (evil-define-key 'normal 'haskell-mode-map
                 (kbd "e") 'subword-forward
                 (kbd "E") 'forward-symbol
                 (kbd "w") 'subword-backward
                 (kbd "W") 'backward-symbol
                 )
               (if (fboundp 'electric-indent-local-mode)
                   (electric-indent-mode -1))
               (haskell-auto-insert-module-template)
               ))

  (add-hook 'haskell-cabal-mode-hook '(lambda () (require 'haskell-mode)))

  ;; align rules for Haskell
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode)))))

  (add-hook 'haskell-error-mode-hook '(lambda () (help-mode))))

(provide 'init-haskell)
;;; init-haskell.el ends here

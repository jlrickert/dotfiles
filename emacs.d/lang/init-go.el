;;; init-go.el --- my go config
;;; Commentary:

;;; Code:
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            '(lambda ()
               (add-hook 'before-save-hook 'gofmt-before-save)
               (smartparens-mode t)
               (setq tab-width 4
                     indent-tabs-mode 1)))
  (evil-leader/set-key-for-mode go-mode
    "." 'godef-jump
    ))

(use-package go-eldoc
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )

(use-package go-rename
  :after go-mode)

(use-package company-go
  :after go-mode
  :config
  (setq company-go-show-annotation t))

(provide 'init-go)
;;; init-go.el ends here

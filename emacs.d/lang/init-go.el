;;; init-go.el --- my go config
;;; Commentary:

;;; Code:
(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

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

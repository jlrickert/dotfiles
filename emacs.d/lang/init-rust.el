;;; init-rust.el --- My rust mode configuration
;;; Commentary:

;;; Code:
(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (cargo-minor-mode)
              (eldoc-mode 1)
              (add-hook 'after-save-hook 'rust-format-buffer)
              ))
  )

(use-package racer
  :ensure t
  :after rust-mode
  :config
  (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
  (setq racer-rust-src-path "~/.local/src/rust/src")
  (setq company-tooltip-align-annotations t)
  (evil-leader/set-key-for-mode 'rust-mode
    "." 'racer-find-definition
    "," 'pop-tag-mark)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook
            '(lambda ()
               (eldoc-mode 1)
               (company-mode 1)))
  )

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(provide 'init-rust)
;;; init-rust.el ends here

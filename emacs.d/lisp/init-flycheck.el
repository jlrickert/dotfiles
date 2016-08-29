;;; init-flycheck.el --- Initialize Flycheck
;;; Commentary:
;;; Code:
(use-package let-alist
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish " ⓢ"
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Flycheck mode:
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (maybe-require-package 'evil-leader)
                (evil-leader/set-key
                  (kbd "el") 'flycheck-list-errors
                  (kbd "en") 'flycheck-next-error
                  (kbd "ep") 'flycheck-previous-error
                  ))))

  ;; Override default flycheck triggers
  (setq
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-idle-change-delay 0.8
   )

  ;; temporary solution for not being able to ergonomically close
  ;; flycheck error messages if it opens in another buffer instead of
  ;; the minibuffer
  (setq max-mini-window-height 0.5)

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info))

(use-package flycheck-pos-tip
  :ensure t
  :disabled t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here

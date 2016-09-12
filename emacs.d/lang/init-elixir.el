;;; init-elixer.el --- My elixer config
;;; Commentary:

;;; Code:
(use-package elixir-mode
  :config
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate)))

  (add-hook 'elixir-mode-hook 'smartparens-mode)
  (after-load 'company
    (push "_build" projectile-globally-ignored-directories))
  )

(use-package alchemist
  :after elixir-mode
  ;; (setq alchemist-project-compile-when-needed t)
  ;; (push 'alchemist-company company-backends-elixir-mode)
  ;; (push 'alchemist-company company-backends-alchemist-iex-mode)

  :config
  (setq alchemist-hooks-test-on-save t
        alchemist-hooks-compile-on-save t
        alchemist-test-ask-about-save nil
        )

  (add-hook 'elixir-mode-hook 'alchemist-mode)

  ;; (evil-leader/set-key-for-mode 'elixir-mode
  ;; "." 'alchemist-
  ;; )

  (dolist (mode (list alchemist-compile-mode-map
                      alchemist-eval-mode-map
                      alchemist-execute-mode-map
                      alchemist-message-mode-map
                      alchemist-help-minor-mode-map
                      alchemist-mix-mode-map
                      alchemist-macroexpand-mode-map
                      alchemist-refcard-mode-map
                      alchemist-test-report-mode-map))
    (evil-define-key 'normal mode
      (kbd "s") 'alchemist-test-toggle-test-report-display
      (kbd "q") 'quit-window)))

(use-package ruby-end
  :after elixir-mode
  :init
  (add-hook 'elixir-mode-hook
            '(lambda ()
               (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
               (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers)
                    nil))
            (ruby-end-mode +1))

  ;; hack to remove the autoloaded `add-hook' in `ruby-end'
  ;; since they are inserted as an autoload, they have to be removed both
  ;; before and after loading
  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)

  :config
  ;; see comment in `:init' block
  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)
  )

(provide 'init-elixir)
;;; init-elixir.el ends here

;;; init-ruby.el --- My personal ruby settings
;;; Commentary:

;;; Code:
(use-package enh-ruby-mode
  :mode (("Appraisals\\'" . enh-ruby-mode)
         ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
         ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :config
  (setq enh-ruby-deep-indent-paren nil
        enh-ruby-hanging-paren-deep-indent-level 2)
  (add-hook 'enh-ruby-mode 'smartparens-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(use-package rhtml-mode
  :config
  ;; (sp-with-modes '(rhtml-mode)
  ;; (sp-local-pair "<" ">")
  ;; (sp-local-pair ""))
  )

(provide 'init-ruby)
;;; init-ruby.el ends here

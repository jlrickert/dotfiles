;;; init-markdown.el --- My markdown configurations
;;; Commentary:

;;; Code:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc -s --mathjax --to html")
  ;; (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  )

(use-package markdown-preview-mode
  :ensure t
  :after (markdown-mode)
  :config
  (add-hook 'markdown-preview-mode-hook
            (lambda ()
              (flyspell-mode t)
              (setq markdown-preview-template
                    (expand-file-name
                     "~/.emacs.d/markdown-preview-template.html"
                     user-emacs-directory))
              (setq markdown-preview-style
                    "http://aaronbieber.com/assets/styles/github-markdown.css"))))

(provide 'init-markdown)
;;; init-markdown.el ends here

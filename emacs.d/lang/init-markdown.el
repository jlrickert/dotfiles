;;; init-markdown.el --- My markdown configurations
;;; Commentary:

;;; Code:
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (let (markdown-css "https://mirror.jlrickert.me/pandoc-md.css")
    (setq markdown-command (format "pandoc -s --mathjax --to html --css=%s" markdown-css)))
  ;; (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  )

(format "%s %s" "a" "b")

(use-package markdown-preview-mode
  :ensure t
  :after (markdown-mode)
  :config
  (add-hook 'markdown-preview-mode-hook
            (lambda ()
              (flyspell-mode t)
              (setq markdown-preview-template
                    (expand-file-name
                     "~/.emacs.d/resources/markdown-preview-template.html"
                     user-emacs-directory))
              (setq markdown-preview-style
                    "http://aaronbieber.com/assets/styles/github-markdown.css"))))

(provide 'init-markdown)
;;; init-markdown.el ends here

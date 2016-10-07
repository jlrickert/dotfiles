;;; init-markdown.el --- My markdown configurations
;;; Commentary:

;;; Code:
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (let ((markdown-css "https://mirror.jlrickert.me/pandoc-md.css"))
    (setq markdown-command (format "pandoc -s --toc --mathjax --to html --css=%s" markdown-css)))
  :config
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (auto-fill-mode t)
               (flyspell-mode t))))

(use-package markdown-preview-mode :after (markdown-mode))


(provide 'init-markdown)
;;; init-markdown.el ends here

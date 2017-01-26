;;; init-js.el --- My javascript IDE configurations
;;; Commentary:

;;; Code:
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.json\\'" . js2-mode)
         )
  :config
  (setq js-indent-level 2
        js2-indent-level 2
        js2-basic-offset 2)
  (add-hook 'js2-mode-hook
            '(lambda ()
               (smartparens-mode t))))

(use-package npm-mode)

(use-package jade-mode
  :mode (("\\.styl$\\'" . sws-mode)
         ("\\.jade\\'" . jade-mode))
  :config
  (require 'sws-mode))

(use-package stylus-mode
  :after jade-mode)

(use-package web-mode
  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2))

(provide 'init-js)
;;; init-js.el ends here

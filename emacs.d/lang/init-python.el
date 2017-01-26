;;; init-python.el --- My personal python settings
;;; Commentary:

;;; Code:
(require 'my-defun)

(use-package python
  :config
  (evil-leader/set-key-for-mode 'python-mode
    "p" 'python-shell-send-defun
    "," 'pop-tag-mark)
  (after-load 'smartparens
    (add-hook 'python-mode-hook 'smartparens-mode)))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook
            '(lambda ()
               (eldoc-mode t)
               (anaconda-eldoc-mode t)))
  (add-hook 'anaconda-mode-view-mode-hook 'help-mode)
  (evil-leader/set-key-for-mode 'python-mode
    "." 'anaconda-mode-find-definitions
    "k" 'anaconda-mode-show-doc
    )
  )

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf))
  )

;; (load "pylookup")
(use-package pylookup
  :ensure nil
  :load-path "site-lisp/pylookup"
  :commands (pylookup-lookup pylookup-update pylookup-update-all)
  :config
  (setq pylookup-dir (jlr/path-join
                      user-emacs-directory
                      "site-lisp"
                      "pylookup")

        pylookup-program (concat pylookup-dir "pylookup.py")
        pylookup-db-file (concat pylookup-dir "pylookup.db")
        pylookup-completing-read 'completing-read)
  (evil-leader/set-key-for-mode 'python-mode
    "h" 'pylookup-lookup)
  )

;;; Ansible setup
(use-package yaml-mode)
(use-package company-ansible
  :config
  (add-to-list 'company-backends 'company-ansible))

(provide 'init-python)
;;; init-python.el ends here

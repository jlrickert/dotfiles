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

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (use-package py-autopep8)

;;   (defun python-reinstate-current-project ()
;;     "When running Python, add the current directory ('') to the head of sys.path.
;; For reasons unexplained, run-python passes arguments to the
;; interpreter that explicitly remove '' from sys.path. This means
;; that, for example, using `python-send-buffer' in a buffer
;; visiting a module's code will fail to find other modules in the
;; same directory.
;; Adding this function to `inferior-python-mode-hook' reinstates
;; the current directory in Python's search path."
;;     (python-shell-internal-send-string
;;      (format "import sys; sys.path.insert(0, %s)" (project-root)))
;;     ;; (python-shell-internal-send-string "sys.path[0:0] = ['']")
;;     )

;;   (defun python-shell-send-line-or-region (&optional prefix)
;;     "Send statement or region to python interpreter"
;;     (interactive "P")
;;     (save-excursion
;;       (if (and (mark) (use-region-p))
;;           (elpy-shell-send-region-or-buffer)
;;         (elpy-shell-send-current-statement))))


;;   (after-load 'company
;;     (push "__pycache__" projectile-globally-ignored-directories))

;;   ;; (elpy-enable)
;;   (elpy-use-ipython)
;;   ;; (venv-initialize-interactive-shells)
;;   ;; (venv-initialize-eshell)
;;   (setq elpy-rpc-backend "jedi"
;;         python-shell-interpreter "ipython"
;;         python-shell-interpreter-args "--simple-prompt -i")

;;   (evil-leader/set-key-for-mode 'python-mode
;;     "," 'pop-tag-mark
;;     "." #'elpy-goto-definition
;;     "k" 'elpy-doc
;;     "p" #'python-shell-send-line-or-region
;;     "rr" 'python-shell-send-buffer
;;     )

;;   ;; (add-hook 'inferior-python-mode-hook #'python-reinstate-current-project)
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               ;; I'm rudely redefining this function to do a comparison of `point'
;;               ;; to the end marker of the `comint-last-prompt' because the original
;;               ;; method of using `looking-back' to match the prompt was never
;;               ;; matching, which hangs the shell startup forever.
;;               (defun python-shell-accept-process-output (process &optional timeout regexp)
;;                 "Redefined to actually work."
;;                 (let ((regexp (or regexp comint-prompt-regexp)))
;;                   (catch 'found
;;                     (while t
;;                       (when (not (accept-process-output process timeout))
;;                         (throw 'found nil))
;;                       (when (= (point) (cdr (python-util-comint-last-prompt)))
;;                         (throw 'found t))))))

;;               ;; Additional settings follow.
;;               (elpy-mode t)
;;               (eldoc-mode)
;;               (add-to-list 'write-file-functions 'delete-trailing-whitespace)
;;               (smartparens-mode t)
;;               (require 'project-root)
;;               (when (projectile-project-p)
;;                 (elpy-set-project-root (projectile-project-root)))
;;               ))
;;   )

(provide 'init-python)
;;; init-python.el ends here

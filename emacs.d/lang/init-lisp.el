;;; init-lisp.el --- My config for lisp
;;; Commentary:

;;; Code:
(require 'derived)

(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar my-lisp-modes-hook
  '(smartparens-strict-mode
    evil-smartparens-mode
    eldoc-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run after entering any `lisp-mode'.")

(defun sanityinc/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and
         (buffer-file-name)
         (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode t)))

(add-hook 'emacs-lisp-mode-hook 'sanityinc/maybe-set-bundled-elisp-readonly)
;; ----------------------------------------------------------------------------
;; Setup Elisp
;; ----------------------------------------------------------------------------
(defconst elisp-modes
  '(emacs-lisp-mode ielm-mode lisp-interaction-mode)
  "Major modes relating to elisp.")

(use-package ipretty
  :ensure t
  :commands eval-last-sexp-or-region
  :preface
  (defun eval-last-sexp-or-region (prefix)
    "Evaluate region if highlighted, otherwise evaluate the PREFIX."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (eval-last-sexp prefix))))

(use-package hl-sexp
  :ensure t
  :config
  (add-to-list 'my-lisp-modes-hook 'hl-sexp-mode))

(use-package elisp-slime-nav
  :ensure t
  :commands jump-to-elisp-docs
  :config
  (defun jump-to-elisp-docs (sym-name)
    "Jump to a pane and do elisp-slime-nav-describe-elisp-thing-at-point with SYM-NAME."
    (interactive (list (elisp-slime-nav--read-symbol-at-point)))
    (help-xref-interned (intern sym-name))
    (switch-to-buffer-other-window "*Help*" t))
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  )

(use-package aggressive-indent
  :ensure t
  :config
  (add-to-list 'my-lisp-modes-hook 'aggressive-indent-mode))

(use-package eldoc-eval
  :ensure t
  :config
  (if (boundp 'eval-expression-minibuffer-setup-hook)
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    (eldoc-in-minibuffer-mode t)))

(add-hook 'emacs-lisp-mode-hook '(lambda () (run-hooks 'my-lisp-modes-hook)))
(add-hook 'lisp-mode '(lambda () (run-hooks 'my-lisp-modes-hook)))

(dolist (mode elisp-modes)
  (evil-leader/set-key-for-mode mode
    "k"  'jump-to-elisp-docs
    "p"  'eval-last-sexp-or-region
    "er" 'eval-and-replace
    "ee" 'eval-buffer
    "." 'find-function-at-point)

  (eval
   `(evil-define-key 'insert ,(derived-mode-map-name mode)
      (kbd ";") 'electric-lisp-comment))
  (eval
   `(evil-define-key 'normal ,(derived-mode-map-name mode)
      (kbd "M-=") 'sp-indent-defun
      (kbd "M-c") 'sp-convolute-sexp
      (kbd "M-d") 'sp-splice-sexp
      (kbd "M-h") 'sp-backward-slurp-sexp
      (kbd "M-j") 'sp-join-sexp
      (kbd "M-l") 'sp-backward-barf-sexp
      (kbd "M-n") 'sp-forward-slurp-sexp
      (kbd "M-p") 'sp-forward-barf-sexp
      (kbd "M-s") 'sp-split-sexp
      (kbd "M-t") 'sp-transpose-sexp
      )))

(provide 'init-lisp)
;;; init-lisp.el ends here

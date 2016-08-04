;;; init-common-lisp.el --- My common lisp configuration
;;; Commentary:

;;; Code:
(require 'derived)
(require 'init-lisp)

(use-package slime
  :ensure t
  :config
  (defconst common-lisp-modes
    '(lisp-mode slime-mode common-lisp-mode)
    "Major modes related to common-lisp")

  (defun maybe-start-slime ()
    "Starts slime if there is no running repl."
    (interactive)
    (unless (slime-connected-p)
      (slime)))

  (defun slime-eval-defun-or-region (prefix)
    "Evalutate region if highlighted, otherwise evaluate defun."
    (maybe-start-slime)
    (if (and (mark) (use-region-p))
        (slime-eval-region (min (point) (mark)) (max (point) (mark)))
      (slime-eval-defun)))

  (defun slime-description-fontify ()
    "Fontify sections of SLIME Description."
    (with-current-buffer "*slime-description*"
      (help-mode)
      (highlight-regexp
       (concat "^Function:\\|"
               "^Macro-function:\\|"
               "^Its associated name.+?) is\\|"
               "^The .+'s arguments are:\\|"
               "^Function documentation:$\\|"
               "^Its.+\\(is\\|are\\):\\|"
               "^On.+it was compiled from:$")
       'hi-green-b)))

  (defadvice slime-show-description (after slime-description-fontify activate)
    "Fontify sections of slime description"
    (slime-description-fontify))

  (use-package slime-company :ensure t)
  (load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-scratch
                         slime-company))

  (evil-set-initial-state 'slime-repl-mode 'insert)
  (dolist (mode common-lisp-modes)
    (evil-leader/set-key-for-mode mode
      "ee" 'slime-eval-buffer
      "," 'slime-pop-find-definition-stack
      "." 'slime-edit-definition
      "k" 'slime-documentation
      "p" 'slime-eval-defun-or-region
      )

    (add-hook (derived-mode-hook-name mode) '(lambda () (message "running")(run-hooks 'my-lisp-modes-hook)))
    (eval
     `(evil-define-key 'insert ,(derived-mode-map-name mode)
        ";" 'electric-lisp-comment))))

(provide 'init-common-lisp)
;;; init-common-lisp.el ends here

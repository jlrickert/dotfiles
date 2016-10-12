;;; init-racket.el --- my config for the racket
;;; Commentary:

;;; Code:
(use-package racket-mode
  :config
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'racket-mode "'" nil :actions nil)
    (sp-local-pair 'racket-mode "`" nil :actions nil))

  (defun jlr/racket-test ()
    "Call `racket-test' with universal argument."
    (interactive)
    (racket-test t))


  (defun jlr/racket-send-last-sexp-or-region (&optional prefix)
    "Send statement or region to python interpreter"
    (interactive "P")
    (save-excursion
      (if (and (mark) (use-region-p))
          (racket-send-region (min (point) (mark)) (max (point) (mark)))
        (racket-send-last-sexp))))

  (add-hook 'racket-mode
            '(lambda ()
               (smartparens-mode t)
               (evil-smartparens-mode t)
               ))

  (evil-leader/set-key-for-mode 'racket-mode
    "M-=" 'sp-indent-defun
    "M-c" 'sp-convolute-sexp
    "M-d" 'sp-splice-sexp
    "M-h" 'sp-backward-slurp-sexp
    "M-j" 'sp-join-sexp
    "M-l" 'sp-backward-barf-sexp
    "M-n" 'sp-forward-slurp-sexp
    "M-p" 'sp-forward-barf-sexp
    "M-s" 'sp-split-sexp
    "M-t" 'sp-transpose-sexp)
  )


(provide 'init-racket)
;;; init-racket.el ends here

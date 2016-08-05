;;; init-clojure.el --- My clojure settings
;;; Commentary:

;;; Code:
(require 'derived)
(require 'init-lisp)

(defconst clojure-modes
  '(clojure-mode clojurescript-mode clojurex-mode clojurec-mode)
  "Majore modes related to clojure-mode.")

(use-package clojure-mode
  :ensure t
  :config
  ;; (use-package flyckeck-clojure :after cider :ensure t)
  (use-package cljsbuild-mode :ensure t)
  (use-package elein :ensure t)
  (use-package cider
    :ensure t
    :config
    (setq nrepl-popup-stacktraces nil
          cider-repl-use-pretty-printing t
          cider-repl-use-clojure-font-lock t
          cider-repl-result-prefix ";; => "
          cider-repl-wrap-history t
          cider-repl-history-size 3000
          cider-prompt-for-symbol nil
          cider-show-error-buffer nil
          cider-stacktrace-default-filters '(tooling dup)
          cider-repl-pop-to-buffer-on-connect nil
          cider-prompt-save-file-on-load nil
          cider-prompt-for-symbol nil
          )
    (add-hook 'cider-mode-hook 'eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

    (evil-set-initial-state 'cider-repl-mode 'insert)
    (evil-set-initial-state 'cider-stacktrace-mode 'motion)
    (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
    (add-hook 'cider-docview-mode-hook '(lambda () (help-mode)))
    )

  (defun clojure/fancify-symbols (mode)
    "Pretty symbols for Clojure's anonymous functions and sets,
   like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
    (font-lock-add-keywords mode
                            `(("(\\(fn\\)[\[[:space:]]"
                               (0 (progn (compose-region (match-beginning 1)
                                                         (match-end 1) "λ"))))
                              ("(\\(partial\\)[\[[:space:]]"
                               (0 (progn (compose-region (match-beginning 1)
                                                         (match-end 1) "Ƥ"))))
                              ("(\\(comp\\)[\[[:space:]]"
                               (0 (progn (compose-region (match-beginning 1)
                                                         (match-end 1) "∘"))))
                              ("\\(#\\)("
                               (0 (progn (compose-region (match-beginning 1)
                                                         (match-end 1) "ƒ"))))
                              ("\\(#\\){"
                               (0 (progn (compose-region (match-beginning 1)
                                                         (match-end 1) "∈")))))))

  (dolist (mode clojure-modes)
    (clojure/fancify-symbols mode)
    (evil-leader/set-key-for-mode mode
      "ee" 'cider-load-buffer
      "er" 'cider-eval-region
      "k" 'cider-doc
      "p" 'cider-eval-defun-at-point)
    (add-hook (derived-mode-hook-name mode) '(lambda () (run-hooks 'my-lisp-modes-hook)))
    (eval `(evil-define-key 'insert ,(derived-mode-map-name mode)
             ";" 'electric-lisp-comment))))

(provide 'init-clojure)
;;; init-clojure.el ends here

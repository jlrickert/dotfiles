;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "SPC")
  (setq evil-leader/in-all-states 1)
  (defun open-shell ()
    "Open up a shell terminal"
    (interactive)
    (ansi-term (getenv "SHELL")))
  (defun reload-gui ()
    "Redraws the gui"
    (interactive)
    (font-lock-fontify-buffer)
    (redraw-display))
  (evil-leader/set-key
    ","  'pop-tag-mark
    "/"  'evilnc-comment-or-uncomment-lines
    ":"  'smex
    ";"  'smex
    "B"  'magit-blame-toggle
    "D"  'open-current-line-in-codebase-search
    "R"  'reload-gui
    "S"  'delete-trailing-whitespace
    "T"  'gtags-find-tag
    "`"  'open-shell
    "a=" 'my-align-single-equals
    "aa" 'align-regexp
    "b"  'switch-to-buffer
    "c"  'comment-dwim
    "d"  'kill-this-buffer
    "ff" 'quick-find-file
    "g"  'magit-status
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'whitespace-mode       ;; Show invisible characters
    "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    "nw" 'widen
    "o"  'make-frame
    "r"  'chrome-reload
    "s"  'ag-project            ;; Ag search from project's root
    "t"  'gtags-reindex
    "w"  'save-buffer
    "x"  'smex
    "y"  'yank-to-x-clipboard)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  (evil-define-motion evil-search-symbol-forward (count &optional symbol)
    "Search forward for symbol under point"
    :jump t
    :type exclusive
    (interactive
     (list
      (prefix-numeric-value current-prefix-arg)
      evil-symbol-word-search
      ))
    (dotimes (var (or count 1))
      (evil-search-word t nil t)))

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  ))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;;; Global bindings.
  (define-key evil-normal-state-map (kbd "M-h") #'sp-backward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "M-l") #'sp-backward-barf-sexp)
  (define-key evil-normal-state-map (kbd "M-p") #'sp-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd "M-n") #'sp-forward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "M-t") #'sp-transpose-sexp)
  (define-key evil-normal-state-map (kbd "M-d") #'sp-splice-sexp)
  (define-key evil-normal-state-map (kbd "M-s") #'sp-split-sexp)
  (define-key evil-normal-state-map (kbd "M-j") #'sp-join-sexp)
  (define-key evil-normal-state-map (kbd "M-=") #'sp-indent-defun)
  (define-key evil-normal-state-map (kbd "M-c") #'sp-convolute-sexp)
  (define-key evil-normal-state-map (kbd "M-d") #'sp-splice-sexp)

  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-]") 'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "S-SPC") 'air-pop-to-org-agenda)
  (define-key evil-normal-state-map (kbd "[i") 'show-first-occurrence)
  (define-key evil-normal-state-map (kbd "b") 'evilmi-jump-items)
  (define-key evil-normal-state-map (kbd "g/") 'occur-last-search)
  (define-key evil-normal-state-map (kbd "q") 'evil-search-symbol-forward)
  (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

  (define-key evil-visual-state-map (kbd "b") 'evilmi-jump-items)
  (define-key evil-visual-state-map (kbd ":") 'exil-ex)
  (define-key evil-visual-state-map (kbd ";") 'exil-ex)

  (define-key evil-motion-state-map (kbd "b") 'evilmi-jump-items)
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "e") 'evil-forward-word-begin)
  (define-key evil-motion-state-map (kbd "E") 'forward-symbol)
  (define-key evil-motion-state-map (kbd "w") 'backward-word)
  (define-key evil-motion-state-map (kbd "W") 'backward-symbol)

  (define-key evil-inner-text-objects-map (kbd "b") 'evilmi-inner-text-object)
  (define-key evil-inner-text-objects-map (kbd "e") 'evil-inner-word)
  (define-key evil-inner-text-objects-map (kbd "E") 'evil-inner-WORD)
  (define-key evil-inner-text-objects-map (kbd "w") 'evil-inner-word)
  (define-key evil-inner-text-objects-map (kbd "W") 'evil-inner-WORD)

  (define-key evil-outer-text-objects-map (kbd "b") 'evilmi-outer-text-object)
  (define-key evil-outer-text-objects-map (kbd "e") 'evil-a-word)
  (define-key evil-outer-text-objects-map (kbd "E") 'evil-a-word)
  (define-key evil-outer-text-objects-map (kbd "w") 'evil-a-word)
  (define-key evil-outer-text-objects-map (kbd "W") 'evil-a-word)

  (evil-define-key 'insert global-map (kbd "s-d") 'eval-last-sexp)
  (evil-define-key 'normal global-map (kbd "s-d") 'eval-defun)
  (evil-define-key 'normal global-map (kbd "z d") 'dictionary-lookup-definition)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (defun evil-visual-line--mark-org-element-when-heading ()
    (interactive)
    (if (and (derived-mode-p 'org-mode)
             (org-on-heading-p))
        (not (org-mark-element))
      t))

  (advice-add 'evil-visual-line :before-while #'evil-visual-line--mark-org-element-when-heading)

  ;; My own Ex commands.
  (evil-ex-define-cmd "q[uit]" 'evil-quit)
  (evil-ex-define-cmd "qa" 'evil-quit-all))

(defun air--apply-evil-other-package-configs ()
  "Apply evil-dependent settings specific to other packages."

  (defun next-conflict-marker ()
    (interactive)
    (evil-next-visual-line)
    (if (not (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t))
        (evil-previous-visual-line))
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; PHP
  (evil-define-key 'normal php-mode-map (kbd "]n") 'next-conflict-marker)
  (evil-define-key 'normal php-mode-map (kbd "[n") 'previous-conflict-marker)
  (evil-define-key 'visual php-mode-map (kbd "]n") 'next-conflict-marker)
  (evil-define-key 'visual php-mode-map (kbd "[n") 'previous-conflict-marker)

  ;; Dired
  (evil-define-key 'normal dired-mode-map (kbd "C-e") 'dired-toggle-read-only))

(defmacro define-evil-or-global-key (key def &optional state)
  "Define a key KEY with DEF in an Evil map, or in the global map.

If the Evil map for STATE is defined (or `normal' if STATE is not
provided) the key will be defined in that map.  Failing that, it will
be defined globally.

Note that STATE should be provided as an unquoted symbol.

This macro provides a way to override Evil mappings in the appropriate
Evil map in a manner that is compatible with environments where Evil
is not used."
  (let* ((evil-map-name (if state
                            (concat "evil-" (symbol-name state) "-state-map")
                          "evil-normal-state-map"))
         (map (if (boundp (intern evil-map-name))
                  (intern evil-map-name)
                global-map)))
    `(define-key ,map ,key ,def)))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (air--config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject :ensure t)
  (use-package evil-nerd-commenter :ensure t)

  (use-package evil-matchit
    :ensure t
    :config
    (global-evil-matchit-mode 1))


  (air--apply-evil-other-package-configs))

(provide 'init-evil)
;;; init-evil.el ends here

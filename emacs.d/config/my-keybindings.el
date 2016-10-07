;;; my-keybindings.el --- my global keybindings
;;; Commentary:

;;; Code:
(require 'my-defun)
(require 'evil)

(evil-define-motion evil-search-symbol-forward (count &optional symbol)
  "Search forward for symbol under point"
  :jump t
  :type exclusive
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    evil-symbol-word-search))
  (dotimes (var (or count 1))
    (evil-search-word t nil t)))


(after-load 'evil-leader
  (evil-leader/set-key
    "," 'pop-tag-mark
    "/" 'evilnc-comment-or-uncomment-lines
    ":" 'smex
    ";" 'smex
    "=" 'jlr/indent-buffer
    "B" 'magit-blame-toggle
    "D" 'open-current-line-in-codebase-search
    "R" 'reload-gui
    "S" 'delete-trailing-whitespace
    "T" 'gtags-find-tag
    "`" 'open-shell
    "a=" 'my-align-single-equals
    "aa" 'align-regexp
    "bb" 'switch-to-buffer
    "bk" 'kill-this-buffer
    "ff" 'quick-find-file
    "fa" 'jlr/ag
    "g" 'magit-status
    "h" 'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l" 'whitespace-mode       ;; Show invisible characters
    "o" 'make-frame
    "ss" 'ag-project           ;; Ag search from project's root
    "tt" 'gtags-reindex
    "w" 'save-buffer
    "x" 'smex))

(after-load 'evil
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-]") 'gtags-find-tag-from-here)
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
  (define-key evil-motion-state-map (kbd "e") 'forward-word)
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

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; Miscellaneous keybindings
  (define-key evil-normal-state-map (kbd "M-d") 'sp-splice-sexp)

  ;; My own Ex commands.
  (evil-ex-define-cmd "q[uit]" 'evil-quit)
  (evil-ex-define-cmd "qa" 'evil-quit-all)
  )

(provide 'my-keybindings)
;;; my-keybindings.el ends here

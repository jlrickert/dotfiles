;;; init.el --- My emacs init files
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "resources" user-emacs-directory))

;(message (format "init completed in %s" (emacs-init-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporarily disable garbage collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq gc-cons-threshold most-positive-fixnum)
(add-hook
 'after-init-hook
 (lambda ()
   (message "Restoring gc-cons to default value")
   (setq gc-cons-threshold (* 1024 1024))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup my person information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jared L. Rickert"
      user-mail-address "jaredrickert52@gmail.com"
      calendar-latitude 45.6
      calendar-longitude 94.1
      calendar-location-name "St Cloud, MN")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup package managment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pin some packages to specific repositories.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-pinned-packages '((gtags . "marmalade")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load my custom submodules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'my-defun)
(require 'my-evil)
(require 'my-core)
(require 'my-gui)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load i3 integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'i3)
(require 'i3-integration)
(i3-one-window-per-frame-mode-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load programming configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-python)
(require 'init-lisp)
;; (require 'init-common-lisp)
(require 'init-clojure)
(require 'init-haskell)
(require 'init-markdown)
(require 'init-rust)
(require 'init-scheme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load my keybings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    "g" 'magit-status
    "h" 'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l" 'whitespace-mode       ;; Show invisible characters
    "o" 'make-frame
    "ss" 'ag-project           ;; Ag search from project's root
    "tt" 'gtags-reindex
    "w" 'save-buffer
    "x" 'smex))

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

  ;; My own Ex commands.
  (evil-ex-define-cmd "q[uit]" 'evil-quit)
  (evil-ex-define-cmd "qa" 'evil-quit-all)
  )

;;; init.el ends here

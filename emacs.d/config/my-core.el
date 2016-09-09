;;; my-core.el --- My core configuration
;;; Commentary:

;;; Code:
(require 'my-defun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p) ;; 'y' or 'n' is good enough

(setq-default ring-bell-function 'ignore)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; 80 charactor lines
(setq fill-column 80)
(set-default 'fill-column 80)

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performace related things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jlr/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun jlr/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'jlr/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'jlr/minibuffer-exit-hook)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTF8 stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq locale-coding-system 'utf-8) ;; pretty
(set-terminal-coding-system 'utf-8) ;; pretty
(set-keyboard-coding-system 'utf-8) ;; pretty
(set-selection-coding-system 'utf-8) ;; please
(prefer-coding-system 'utf-8) ;; with sugar on top
(set-language-environment "UTF-8")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Editor config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode)
(use-package nlinum
  :config
  (defun initialize-nlinum (&optional frame)
    (require 'nlinum)
    (add-hook 'prog-mode-hook 'nlinum-mode))
  (when (daemonp)
    (add-hook 'window-setup-hook 'initialize-nlinum)
    (defadvice make-frame (around toggle-nlinum-mode compile activate)
      (nlinum-mode -1) ad-do-it (nlinum-mode 1)))
  )
(setq-default truncate-lines t) ;; disables line wrapping
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming related stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Keep cursor away from edges when scrolling up/down
(use-package smooth-scrolling :disabled t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups"))))
      backup-by-copying t
      delete-old-versions t
      make-backup-files t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


;; autsave no longer puts it in my working directory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autcomplete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :diminish "CMP"
  :config
  (global-company-mode)
  (setq-default
   company-backends '((company-capf company-dabbrev-code) company-dabbrev))

  (setq company-idle-delay 0.2)
  (setq completion-cycling 5)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)

  (define-key evil-insert-state-map (kbd "M-n") 'company-complete-common)
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  )

(use-package company-quickhelp
  :after company
  :config
  (add-hook 'company-quickhelp-mode-hook
            '(lambda ()
               (define-key company-active-map (kbd "M-n") 'company-select-next)
               (define-key company-active-map (kbd "M-p") 'company-select-previous)
               ))
  (company-quickhelp-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (key-chord-define-global (kbd "jk") 'yas-expand)
  (key-chord-define yas-keymap (kbd "jk") 'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  ;; Override default flycheck triggers
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (after-load 'evil-leader
                (evil-leader/set-key
                  (kbd "el") 'flycheck-list-errors
                  (kbd "en") 'flycheck-next-error
                  (kbd "ep") 'flycheck-previous-error))))

  ;; temporary solution for not being able to ergonomically close
  ;; flycheck error messages if it opens in another buffer instead of
  ;; the minibuffer
  (setq max-mini-window-height 0.5)

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))

  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)

  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)

  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :config
  (projectile-global-mode)
                                        ;(setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-cache-file (expand-file-name ".projectile" user-emacs-directory)
        projectile-known-projects-file (expand-file-name ".projectile-bookmarks" user-emacs-directory)
        projectile-recentf-files (expand-file-name ".recentf" user-emacs-directory)
        projectile-completion-system 'ido
        projectile-switch-project-action 'projectile-dired
        projectile-globally-ignored-directories '(".metadata"
                                                  ".cabal-sandbox"
                                                  ".stversions"
                                                  ".stfolder"
                                                  ".stignore"
                                                  "dist"
                                                  "elpa"
                                                  "snippets"
                                                  "node_modules"
                                                  "build"
                                                  "target"
                                                  "out"
                                                  "cache"
                                                  ".cache"
                                                  ".git"
                                                  ".hg"
                                                  ".bzr"
                                                  ".svn"
                                                  "venv")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-symbol
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;;; ido
(use-package ido
  :ensure t
  :after smex
  :config
  (require 'uniquify)
  (use-package ido-ubiquitous :ensure t)
  (use-package flx-ido :ensure t :config)
  (use-package ido-vertical-mode :ensure t)

  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (ido-ubiquitous-mode 1)
  (ido-vertical-mode 1)
  (setq ido-enable-prefix nil
        ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-show-dot-for-dired t
        ido-confirm-unique-completion nil
        ido-enable-last-directory-history nil
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length 0)

  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")

  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background "#e5b7c0")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background "#e52b50"
                      :foreground "white")
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground "#b00000")
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)

  (add-hook 'ido-setup-hook
            '(lambda ()
               (define-key ido-file-dir-completion-map "~" 'ido-jump-to-home)
               (define-key ido-file-dir-completion-map (kbd "RET") 'exit-minibuffer)
               (define-key ido-file-dir-completion-map [tab] 'ido-complete)
               (define-key ido-file-dir-completion-map (kbd "M-Y") 'ido-select-text)
               (define-key ido-file-dir-completion-map (kbd "M-n") 'ido-next-match)
               (define-key ido-file-dir-completion-map (kbd "M-p") 'ido-prev-match)
               (define-key ido-file-dir-completion-map (kbd "M-y") 'ido-select-text)

               (define-key ido-common-completion-map (kbd "RET") 'exit-minibuffer)
               (define-key ido-common-completion-map (kbd "TAB") 'ido-complete)
               (define-key ido-common-completion-map (kbd "M-n") 'ido-next-match)
               (define-key ido-common-completion-map (kbd "M-p") 'ido-prev-match)
               (define-key ido-common-completion-map (kbd "M-y") 'ido-select-text)))


  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
           `(lambda()
              (interactive)
              (if (string= " " (this-command-keys))
                  (insert ?-)
                (funcall ,ido-cannot-complete-command)))))
      ad-do-it))
  )


;;; smex
(use-package smex
  :ensure t
  :config
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))

  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize)

  (add-hook 'after-load-functions 'smex-update-after-load)

  (global-set-key (kbd "M-x") 'smex))


;;; guide-key
(use-package guide-key
  :diminish ""
  :config
  (setq guide-key/guide-key-sequence '("C-x" "M-x" "SPC"))
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/idle-delay .2)
  (setq guide-key/recursive-key-sequence-flag t)
  (guide-key-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smartparens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode 1)
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

(use-package evil-smartparens
  :ensure t
  :after (smartparends evil)
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))


(provide 'my-core)
;;; my-core.el ends here
